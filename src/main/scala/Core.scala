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
  })

  // 32本の32bit幅(WORD_LEN.W)レジスタを用意
  val regfile = Mem(32, UInt(WORD_LEN.W))

  /*--------------------------------*/
  /* [IF - Instruction Fetch Stage] */

  // PC レジスタ
  val pc_reg = RegInit(START_ADDR)

  // 取得する命令のアドレスを PC で設定
  io.imem.addr := pc_reg
  // 命令を取得
  val inst = io.imem.inst

  val pc_plus4 = pc_reg + 4.U(WORD_LEN.W)

  // 分岐フラグ
  val br_flg = Wire(Bool())

  // 分岐先
  val br_target = Wire(UInt(WORD_LEN.W))

  // ジャンプフラグ
  val jmp_flg = (inst === JAL || inst === JALR)

  val alu_out = Wire(UInt(WORD_LEN.W))

  val pc_next = MuxCase(
    pc_plus4,
    Seq(
      br_flg -> br_target, // 分岐フラグがONなら分岐先を接続
    )
  )

  pc_reg := pc_next

  /*---------------------------------*/
  /* [ID - Instruction Decode Stage] */

  val rs1_addr = inst(19, 15) // rs1
  val rs2_addr = inst(24, 20) // rs2
  val wb_addr  = inst(11, 7)  // rd - Write-Back用

  // rs1 のデータを取得(無効なアドレスなら0.U)
  val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.U)), regfile(rs1_addr), 0.U(WORD_LEN.W))

  // rs2 のデータを取得(無効なアドレスなら0.U)
  val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.U)), regfile(rs2_addr), 0.U(WORD_LEN.W))

  /* I形式の命令の即値 */
  val imm_i = inst(31, 20)
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  /* S形式の命令の即値 */
  val imm_s = Cat(inst(31, 25), inst(11, 7))
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

  /* B形式の命令の即値 */
  val imm_b = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8))
  val imm_b_sext = Cat(Fill(19, imm_b(11)), imm_b, 0.U(1.U))

  /* J形式の命令の即値 */
  val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21))
  val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.U)) // 最下位bitを0にする。

  /* U形式の命令の即値 */
  val imm_u = inst(31, 12)
  val imm_u_shifted = Cat(imm_u, Fill(12, 0.U))

  val csignals = ListLookup(
    inst,
    List(          ALU_X,   OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
    Array(
      LW   -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM), // sext(M[x[rs1] + sext(offset)][31:0])
      SW   -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  ), // M[x[rs1] + sext(offset)] = x[rs2][31:0]

      ADD  -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // x[rs1] + x[rs2]
      ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU), // x[rs1] + sext(immediate)
      SUB  -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // x[rs1] - x[rs2]

      AND  -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // x[rs1] & x[rs2]
      OR   -> List(ALU_OR,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // x[rs1] | x[rs2]
      XOR  -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // x[rs1] ^ x[rs2]

      ANDI  -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU), // x[rs1] & sext(immediate)
      ORI   -> List(ALU_OR,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU), // x[rs1] | sext(immediate)
      XORI  -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU), // x[rs1] ^ sext(immediate)

      SLL -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      SRL -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      SRA -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),

      SLLI -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      SRLI -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      SRAI -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

      SLT  -> List(ALU_SLT,  OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
      SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),

      SLTI  -> List(ALU_SLT,  OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
      SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

      BEQ  -> List(BR_BEQ,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
      BNE  -> List(BR_BNE,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
      BGE  -> List(BR_BGE,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
      BGEU -> List(BR_BGEU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
      BLT  -> List(BR_BLT,  OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
      BLTU -> List(BR_BLTU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),

      JAL  -> List(ALU_ADD,  OP1_PC,  OP2_IMJ, MEN_X, REN_S, WB_PC),
      JALR -> List(ALU_JALR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC),

      LUI   -> List(ALU_ADD,  OP1_X,   OP2_IMU, MEN_X, REN_S, WB_ALU),
      AUIPC -> List(ALU_ADD,  OP1_PC,  OP2_IMU, MEN_X, REN_S, WB_ALU),
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
    */
  val exe_fun :: op1_sel :: op2_sel :: mem_wen ::rf_wen :: wb_sel :: Nil = csignals

  val op1_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (op1_sel === OP1_RS1) -> rs1_data,
      (op1_sel === OP1_PC)  -> pc_reg,
    )
  )
  val op2_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (op2_sel === OP2_RS2) -> rs2_data,
      (op2_sel === OP2_IMI) -> imm_i_sext,
      (op2_sel === OP2_IMS) -> imm_s_sext,
      (op2_sel === OP2_IMJ) -> imm_j_sext,
      (op2_sel === OP2_IMU) -> imm_u_shifted,
    )
  )

  /*----------------------*/
  /* [EX - Execute Stage] */

  // 演算した結果を alu_out に接続
  alu_out := MuxCase(
    0.U(WORD_LEN),
    Seq(
      (exe_fun === ALU_ADD) -> (op1_data + op2_data),
      (exe_fun === ALU_SUB) -> (op1_data - op2_data),

      (exe_fun === ALU_AND) -> (op1_data & op2_data),
      (exe_fun === ALU_OR)  -> (op1_data | op2_data),
      (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),

      (exe_fun === ALU_SLL) -> (op1_data << op2_data(4, 0))(31, 0),
      (exe_fun === ALU_SRL) -> (op1_data >> op2_data(4, 0)).asUInt(),
      (exe_fun === ALU_SRA) -> (op1_data.asSInt() >> op2_data(4, 0)).asUInt(),

      (exe_fun === ALU_SLT)  -> (op1_data.asSInt() < op2_data.asSInt()).asUInt(),
      (exe_fun === ALU_SLTU) -> (op1_data < op2_data).asUInt(),

      (exe_fun === ALU_JALR) -> ((op1_data + op2_data) & ~1.U(WORD_LEN.W)),
    )
  )

  // 分岐命令
  br_flg := MuxCase(
    false.B,
    Seq(
      (exe_fun === BR_BEQ)  ->  (op1_data === op2_data),
      (exe_fun === BR_BNE)  -> !(op1_data === op2_data),
      (exe_fun === BR_BLT)  ->  (op1_data.asSInt() < op2_data.asSInt()),
      (exe_fun === BR_BGE)  -> !(op1_data.asSInt() < op2_data.asSInt()),
      (exe_fun === BR_BLTU) ->  (op1_data < op2_data),
      (exe_fun === BR_BGEU) -> !(op1_data < op2_data),
    )
  )
  br_target := pc_reg + imm_b_sext

  /*-----------------------------*/
  /* [MEM - Memory Access Stage] */

  io.dmem.addr := alu_out // 算出したメモリアドレスをデータ用メモリのアドレスに接続

  io.dmem.wen   := mem_wen
  io.dmem.wdata := rs2_data      // 書き込むデータを接続

  /*-------------------------*/
  /* [WB - Write-Back Stage] */

  val wb_data = MuxCase(
    alu_out, // WB_ALU が定義されているが alu_out はデフォルトの接続元にしている。
    Seq(
      (wb_sel === WB_MEM) -> io.dmem.rdata,
      (wb_sel === WB_PC) -> pc_plus4,
    )
  )

  when(rf_wen === REN_S) {
    // wb_addr が示すレジスタに Write-Back する。
    regfile(wb_addr) := wb_data
  }

  io.exit := (inst === 0x00638433.U(WORD_LEN.W))

  /*-------------------------*/
  /*          Debug          */
  printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst       : 0x${Hexadecimal(inst)}\n")

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
