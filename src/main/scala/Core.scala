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

  // 立ち上がりエッジで pc_reg += 4 する回路
  pc_reg := pc_reg + 4.U(WORD_LEN.W)

  /*---------------------------------*/
  /* [ID - Instruction Decode Stage] */

  val rs1_addr = inst(19, 15) // rs1
  val rs2_addr = inst(24, 20) // rs2
  val wb_addr  = inst(11, 7)  // rd - Write-Back用

  /* I形式の命令のデコード */

  val imm_i = inst(31, 20)
  val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i)

  /* S形式の命令のデコード */

  val imm_s = Cat(inst(31, 25), inst(11, 7))
  val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)

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
    )
  )

  /**
    * exe_fun : 使用するALU命令
    * ├─ src/main/scala/common/Consts.scala を参照
    * 
    * op1_sel : 第1オペランド
    * ├─ OP1_RS1 : rs1_data
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
    )
  )
  val op2_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (op2_sel === OP2_RS2) -> rs2_data,
      (op2_sel === OP2_IMI) -> imm_i_sext,
      (op2_sel === OP2_IMS) -> imm_s_sext,
    )
  )

  /**
    * RISC-V では0番レジスタの値は常に0になる。
    */

  // rs1 のデータを取得(無効なアドレスなら0.U)
  val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.U)), regfile(rs1_addr), 0.U(WORD_LEN.W))

  // rs2 のデータを取得(無効なアドレスなら0.U)
  val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.U)), regfile(rs2_addr), 0.U(WORD_LEN.W))

  /*----------------------*/
  /* [EX - Execute Stage] */

  // 演算した結果を alu_out に接続
  val alu_out = MuxCase(
    0.U(WORD_LEN),
    Seq(
      (exe_fun === ALU_ADD) -> (op1_data + op2_data),
      (exe_fun === ALU_SUB) -> (op1_data - op2_data),
      (exe_fun === ALU_AND) -> (op1_data & op2_data),
      (exe_fun === ALU_OR)  -> (op1_data | op2_data),
      (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),
    )
  )

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
    )
  )

  when(rf_wen === REN_S) {
    // wb_addr が示すレジスタに Write-Back する。
    regfile(wb_addr) := wb_data
  }

  io.exit := (inst === 0x00602823.U(WORD_LEN.W))

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
  printf(p"dmem.addr  : ${io.dmem.addr}\n")
  printf(p"dmem.wen   : ${io.dmem.wen}\n")
  printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
  printf("-----------------------------\n")
}
