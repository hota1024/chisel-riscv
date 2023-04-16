package fetch

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
  val io = IO(new Bundle {
    // 命令用メモリの入出力
    val imem = Flipped(new ImemPortIo())

    // データ用メモリの入出力
    val dmem = Flipped(new DmemPortIo())

    // 処理が終わると true.B になる出力
    val exit = Output(Bool())

    // パス条件判定用信号
    val gp = Output(UInt(WORD_LEN.W))
  })

  // 32本の32bit幅(WORD_LEN.W)レジスタを用意
  val regfile = Mem(32, UInt(WORD_LEN.W))
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))

  /*----------------------------*/
  /* [Pipeline State Registers] */

  // IF/ID State
  val id_reg_pc             = RegInit(0.U(WORD_LEN.W))
  val id_reg_inst           = RegInit(0.U(WORD_LEN.W))

  // ID/EX State
  val exe_reg_pc            = RegInit(0.U(WORD_LEN.W))
  val exe_reg_wb_addr       = RegInit(0.U(ADDR_LEN.W))
  val exe_reg_op1_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_op2_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_rs2_data      = RegInit(0.U(WORD_LEN.W))
  val exe_reg_exe_fun       = RegInit(0.U(EXE_FUN_LEN.W))
  val exe_reg_mem_wen       = RegInit(0.U(MEN_LEN.W))
  val exe_reg_rf_wen        = RegInit(0.U(REN_LEN.W))
  val exe_reg_wb_sel        = RegInit(0.U(WB_SEL_LEN.W))
  val exe_reg_csr_addr      = RegInit(0.U(CSR_ADDR_LEN.W))
  val exe_reg_csr_cmd       = RegInit(0.U(CSR_LEN.W))
  val exe_reg_imm_i_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_s_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_b_sext    = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_z_uext    = RegInit(0.U(WORD_LEN.W))

  // EX/MEM State
  val mem_reg_pc            = RegInit(0.U(WORD_LEN.W))
  val mem_reg_wb_addr       = RegInit(0.U(ADDR_LEN.W))
  val mem_reg_op1_data      = RegInit(0.U(WORD_LEN.W))
  val mem_reg_rs2_data      = RegInit(0.U(WORD_LEN.W))
  val mem_reg_mem_wen       = RegInit(0.U(MEN_LEN.W))
  val mem_reg_rf_wen        = RegInit(0.U(REN_LEN.W))
  val mem_reg_wb_sel        = RegInit(0.U(WB_SEL_LEN.W))
  val mem_reg_csr_addr      = RegInit(0.U(CSR_ADDR_LEN.W))
  val mem_reg_csr_cmd       = RegInit(0.U(CSR_LEN.W))
  val mem_reg_imm_z_uext    = RegInit(0.U(WORD_LEN.W))
  val mem_reg_alu_out       = RegInit(0.U(WORD_LEN.W))

  // MEM/WB State
  val wb_reg_wb_addr        = RegInit(0.U(ADDR_LEN.W))
  val wb_reg_rf_wen         = RegInit(0.U(REN_LEN.W))
  val wb_reg_wb_data        = RegInit(0.U(WORD_LEN.W))

  /*--------------------------------*/
  /* [IF - Instruction Fetch Stage] */

  // PC レジスタ
  val if_pc_reg = RegInit(START_ADDR)
  // 取得する命令のアドレスを PC で設定
  io.imem.addr := if_pc_reg
  // 命令を取得
  val if_inst = io.imem.inst

  // 分岐先
  val exe_br_target = Wire(UInt(WORD_LEN.W))
  // 分岐フラグ
  val exe_br_flg = Wire(Bool())
  // ジャンプフラグ
  val exe_jmp_flg = Wire(Bool())
  // 演算結果
  val exe_alu_out = Wire(UInt(WORD_LEN.W))

  val if_pc_plus4 = if_pc_reg + 4.U(WORD_LEN.W)
  val if_pc_next = MuxCase(
    if_pc_plus4,
    Seq(
      exe_br_flg       -> exe_br_target,          // 分岐フラグがONなら分岐先を接続
      exe_jmp_flg      -> exe_alu_out,
      (if_inst === ECALL) -> csr_regfile(0x305), // mtvec(0x305) に trap_vector(例外処理) が格納されている
    )
  )
  if_pc_reg := if_pc_next

  /*------------------*/
  /* [IF/ID Register] */

  id_reg_pc   := if_reg_pc
  id_Reg_inst := if_inst

  /*---------------------------------*/
  /* [ID - Instruction Decode Stage] */

  val id_rs1_addr = inst(19, 15) // rs1
  val id_rs2_addr = inst(24, 20) // rs2
  val id_wb_addr  = inst(11, 7)  // rd - Write-Back用

  // rs1 のデータを取得(無効なアドレスなら0.U)
  val id_rs1_data = Mux((id_rs1_addr =/= 0.U(WORD_LEN.U)), regfile(id_rs1_addr), 0.U(WORD_LEN.W))

  // rs2 のデータを取得(無効なアドレスなら0.U)
  val id_rs2_data = Mux((id_rs2_addr =/= 0.U(WORD_LEN.U)), regfile(id_rs2_addr), 0.U(WORD_LEN.W))

  /* I形式の命令の即値 */
  val id_imm_i = inst(31, 20)
  val id_imm_i_sext = Cat(Fill(20, id_imm_i(11)), id_imm_i)

  /* S形式の命令の即値 */
  val id_imm_s = Cat(id_reg_inst(31, 25), id_reg_inst(11, 7))
  val id_imm_s_sext = Cat(Fill(20, id_imm_s(11)), id_imm_s)

  /* B形式の命令の即値 */
  val id_imm_b = Cat(id_reg_inst(31), id_reg_inst(7), id_reg_inst(30, 25), id_reg_inst(11, 8))
  val id_imm_b_sext = Cat(Fill(19, id_imm_b(11)), id_imm_b, 0.U(1.U))

  /* J形式の命令の即値 */
  val id_imm_j = Cat(id_reg_inst(31), id_reg_inst(19, 12), id_reg_inst(20), id_reg_inst(30, 21))
  val id_imm_j_sext = Cat(Fill(11, id_imm_j(19)), id_imm_j, 0.U(1.U)) // 最下位bitを0にする。

  /* U形式の命令の即値 */
  val id_imm_u = id_reg_inst(31, 12)
  val id_imm_u_shifted = Cat(id_imm_u, Fill(12, 0.U))

  /* CSR用の即値 */
  val id_imm_z = id_reg_inst(19, 15)
  val id_imm_z_uext = Cat(Fill(27, 0.U), id_imm_z)

  val csignals = ListLookup(
    id_reg_inst,
    List(              ALU_X,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
    Array(
      LW     -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X), // sext(M[x[rs1] + sext(offset)][31:0])
      SW     -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X,   CSR_X), // M[x[rs1] + sext(offset)] = x[rs2][31:0]

      ADD    -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] + x[rs2]
      ADDI   -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] + sext(immediate)
      SUB    -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] - x[rs2]

      AND    -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] & x[rs2]
      OR     -> List(ALU_OR,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] | x[rs2]
      XOR    -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] ^ x[rs2]

      ANDI   -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] & sext(immediate)
      ORI    -> List(ALU_OR,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] | sext(immediate)
      XORI   -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X), // x[rs1] ^ sext(immediate)

      SLL    -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRL    -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SRA    -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      SLLI   -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRLI   -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SRAI   -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      SLT    -> List(ALU_SLT,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTU   -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      SLTI   -> List(ALU_SLT,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      SLTIU  -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      BEQ    -> List(BR_BEQ,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),
      BNE    -> List(BR_BNE,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),
      BGE    -> List(BR_BGE,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),
      BGEU   -> List(BR_BGEU,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),
      BLT    -> List(BR_BLT,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),
      BLTU   -> List(BR_BLTU,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X,  CSR_X),

      JAL    -> List(ALU_ADD,  OP1_PC,  OP2_IMJ, MEN_X, REN_S, WB_PC, CSR_X),
      JALR   -> List(ALU_JALR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC, CSR_X),

      LUI    -> List(ALU_ADD,  OP1_X,   OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),
      AUIPC  -> List(ALU_ADD,  OP1_PC,  OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),

      CSRRW  -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRWI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),
      CSRRS  -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRSI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),
      CSRRC  -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),
      CSRRCI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),

      ECALL  -> List(ALU_X,     OP1_X,   OP2_X, MEN_X, REN_X,   WB_X, CSR_E),
    )
  )

  /**
    * exe_fun : 使用するALU命令
    * ├─ src/main/scala/common/Consts.scala を参照
    * 
    * op1_sel : 第1オペランド
    * ├─ OP1_RS1 : rs1_data
    * ├─ OP1_PC  : pc_reg
    * 
    * op2_sel : 第2オペランド
    * ├─ OP2_RS2 : rs2_data
    * ├─ OP2_IMI : imm_i_sext
    * ├─ OP2_IMS : imm_s_sext
    * 
    * mem_wen : 書き込みの有無
    * ├─ MEN_X : 書き込み無し
    * ├─ MEN_S : メモリへの書き込み有り
    * 
    * rf_wen : Write-Back の有無
    * ├─ REN_X : Write-Back 無し
    * ├─ REN_S : メモリへの Write-Back 有り
    * 
    * wb_sel : Write-Back するデータソース
    * ├─ WB_X   : データ無し
    * ├─ WB_ALU : 演算結果(alu_out)
    * ├─ WB_MEM : メモリ(io.dmem.rdata)
    * 
    * csr_cmd : CSR への操作
    * ├─ CSR_X : 操作なし
    * ├─ CSR_W : 書き込み
    * ├─ CSR_S : OR でセット
    * ├─ CSR_C : Not して And でセット
    */
  val id_exe_fun :: id_op1_sel :: id_op2_sel :: id_mem_wen :: id_rf_wen :: id_wb_sel :: id_csr_cmd :: Nil = csignals

  val op1_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op1_sel === OP1_RS1) -> id_rs1_data,
      (id_op1_sel === OP1_PC)  -> id_pc_reg,
      (id_op1_sel === OP1_IMZ) -> id_imm_z_uext,
    )
  )
  val op2_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op2_sel === OP2_RS2) -> id_rs2_data,
      (id_op2_sel === OP2_IMI) -> id_imm_i_sext,
      (id_op2_sel === OP2_IMS) -> id_imm_s_sext,
      (id_op2_sel === OP2_IMJ) -> id_imm_j_sext,
      (id_op2_sel === OP2_IMU) -> id_imm_u_shifted,
    )
  )

  val id_csr_addr = Mux(id_csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), id_reg_inst(31, 20))

  /*-------------------*/
  /* [ID/EX registers] */

  exe_reg_pc            := id_reg_pc
  exe_reg_op1_data      := id_op1_data
  exe_reg_op2_data      := id_op2_data
  exe_reg_rs2_data      := id_rs2_data
  exe_reg_wb_addr       := id_wb_addr
  exe_reg_rf_wen        := id_rf_wen
  exe_reg_exe_fun       := id_exe_fun
  exe_reg_wb_sel        := id_wb_sel
  exe_reg_imm_i_sext    := id_imm_i_sext
  exe_reg_imm_s_sext    := id_imm_s_sext
  exe_reg_imm_b_sext    := id_imm_b_sext
  exe_reg_imm_u_shifted := id_imm_u_shifted
  exe_reg_imm_z_uext    := id_imm_z_uext
  exe_reg_csr_addr      := id_csr_addr
  exe_reg_csr_cmd       := id_csr_cmd
  exe_reg_mem_wen       := id_mem_wen

  /*----------------------*/
  /* [EX - Execute Stage] */

  // 演算した結果を alu_out に接続
  exe_alu_out := MuxCase(
    0.U(WORD_LEN),
    Seq(
      (exe_reg_exe_fun === ALU_ADD) -> (exe_reg_op1_data + exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_SUB) -> (exe_reg_op1_data - exe_reg_op2_data),

      (exe_reg_exe_fun === ALU_AND) -> (exe_reg_op1_data & exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_OR)  -> (exe_reg_op1_data | exe_reg_op2_data),
      (exe_reg_exe_fun === ALU_XOR) -> (exe_reg_op1_data ^ exe_reg_op2_data),

      (exe_reg_exe_fun === ALU_SLL) -> (exe_reg_op1_data << exe_reg_op2_data(4, 0))(31, 0),
      (exe_reg_exe_fun === ALU_SRL) -> (exe_reg_op1_data >> exe_reg_op2_data(4, 0)).asUInt(),
      (exe_reg_exe_fun === ALU_SRA) -> (exe_reg_op1_data.asSInt() >> exe_reg_op2_data(4, 0)).asUInt(),

      (exe_reg_exe_fun === ALU_SLT)  -> (exe_reg_op1_data.asSInt() < exe_reg_op2_data.asSInt()).asUInt(),
      (exe_reg_exe_fun === ALU_SLTU) -> (exe_reg_op1_data < exe_reg_op2_data).asUInt(),

      (exe_reg_exe_fun === ALU_JALR) -> ((exe_reg_op1_data + exe_reg_op2_data) & ~1.U(WORD_LEN.W)),

      (exe_reg_exe_fun === ALU_COPY1) -> exe_reg_op1_data,
    )
  )

  // 分岐命令
  exe_br_flg := MuxCase(
    false.B,
    Seq(
      (exe_reg_exe_fun === BR_BEQ)  ->  (exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BNE)  -> !(exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BLT)  ->  (exe_reg_op1_data.asSInt() < exe_reg_op2_data.asSInt()),
      (exe_reg_exe_fun === BR_BGE)  -> !(exe_reg_op1_data.asSInt() < exe_reg_op2_data.asSInt()),
      (exe_reg_exe_fun === BR_BLTU) ->  (exe_reg_op1_data < exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BGEU) -> !(exe_reg_op1_data < exe_reg_op2_data),
    )
  )
  exe_br_target := exe_reg_pc + exe_reg_imm_b_sext

  /*--------------------*/
  /* [EX/MEM registers] */

  mem_reg_pc         := exe_reg_pc
  mem_reg_op1_data   := exe_reg_op1_data
  mem_reg_rs2_data   := exe_reg_rs2_data
  mem_reg_wb_addr    := exe_reg_wb_addr
  mem_reg_alu_out    := exe_alu_out
  mem_reg_rf_wen     := exe_reg_rf_wen
  mem_reg_wb_sel     := exe_reg_wb_sel
  mem_reg_csr_addr   := exe_reg_csr_addr
  mem_reg_csr_cmd    := exe_reg_csr_cmd
  mem_reg_imm_z_uext := exe_reg_imm_z_uext
  mem_reg_mem_wen    := exe_reg_mem_wen

  /*-----------------------------*/
  /* [MEM - Memory Access Stage] */

  io.dmem.addr  := mem_reg_alu_out       // 算出したメモリアドレスをデータ用メモリのアドレスに接続
  io.dmem.wen   := mem_reg_mem_wen
  io.dmem.wdata := mem_reg_rs2_data      // 書き込むデータを接続

  /* CSR */

  val csr_rdata = csr_regfile(mem_reg_csr_addr)
  val csr_wdata = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (mem_reg_csr_cmd === CSR_W) -> mem_reg_op1_data,
      (mem_reg_csr_cmd === CSR_S) -> (csr_rdata | mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_C) -> (csr_rdata & ~mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_E) -> 11.U(WORD_LEN.W),        // マシンモードからのECALL
    )
  )

  when(mem_reg_csr_cmd > 0.U) {
    csr_regfile(mem_Reg_csr_addr) := csr_wdata
  }

  val mem_wb_data = MuxCase(
    mem_reg_alu_out, // WB_ALU が定義されているが alu_out はデフォルトの接続元にしている。
    Seq(
      (mem_reg_wb_sel === WB_MEM) -> io.dmem.rdata,
      (mem_reg_wb_sel === WB_PC)  -> (mem_reg_pc + 4.U(WORD_LEN.W)),
      (mem_reg_wb_sel === WB_CSR) -> csr_rdata
    )
  )

  /*-------------------------*/
  /* [WB - Write-Back Stage] */

  when(rf_wen === REN_S) {
    // wb_addr が示すレジスタに Write-Back する。
    regfile(wb_addr) := wb_data
  }

  /*-------------------------*/
  /*          Debug          */
  io.gp := regfile(3)
  io.exit := (inst === UNIMP)

  printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst       : 0x${Hexadecimal(inst)}\n")
  printf(p"gp         : ${regfile(3)}\n")

  printf(p"rs1_addr   : $rs1_addr\n")
  printf(p"rs1_data   : 0x${Hexadecimal(rs1_data)}\n")

  printf(p"rs2_addr   : $rs2_addr\n")
  printf(p"rs2_data   : 0x${Hexadecimal(rs2_data)}\n")

  printf(p"wb_addr    : $wb_addr\n")
  printf(p"wb_data    : 0x${Hexadecimal(wb_data)}\n")

  printf(p"dmem.addr  : ${io.dmem.addr}\n")
  printf(p"dmem.rdata : 0x${Hexadecimal(io.dmem.rdata)}\n")
  printf(p"dmem.wen   : ${io.dmem.wen}\n")
  printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
  printf("-----------------------------\n")
}
