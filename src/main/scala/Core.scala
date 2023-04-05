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
    0.U(WORD_LEN), // デフォルト値
    Seq(
      (inst === LW || inst === ADDI) -> (rs1_data + imm_i_sext), // I命令の出力先アドレスを演算
      (inst === SW)                  -> (rs1_data + imm_s_sext), // Store先アドレスを演算
      (inst === ADD)                 -> (rs1_data + rs2_data),   // x[rs1] + x[rs2]
      (inst === SUB)                 -> (rs1_data - rs2_data),   // x[rs1] - x[rs2]

      (inst === AND) -> (rs1_data & rs2_data), // x[rs1] & x[rs2]
      (inst === OR)  -> (rs1_data | rs2_data), // x[rs1] | x[rs2]
      (inst === XOR) -> (rs1_data ^ rs2_data), // x[rs1] ^ x[rs2]

      (inst === ANDI) -> (rs1_data & imm_i_sext), // x[rs1] & sext(imm_i)
      (inst === ORI)  -> (rs1_data | imm_i_sext), // x[rs1] | sext(imm_i)
      (inst === XORI) -> (rs1_data ^ imm_i_sext), // x[rs1] ^ sext(imm_i)
    )
  )

  /*-----------------------------*/
  /* [MEM - Memory Access Stage] */

  io.dmem.addr := alu_out // 算出したメモリアドレスをデータ用メモリのアドレスに接続

  io.dmem.wen   := (inst === SW) // SW 命令なら書き込み信号をON
  io.dmem.wdata := rs2_data      // 書き込むデータを接続

  /*-------------------------*/
  /* [WB - Write-Back Stage] */

  // Write-Back するデータ
  val wb_data = MuxCase(
    alu_out, // デフォルトでは alu_out を選択
    Seq(
      (inst === LW) -> io.dmem.rdata, // LW ならデータを読み込む
    )
  )

  when(
       inst === LW
    // 加減演算
    || inst === ADD
    || inst === ADDI
    || inst === SUB

    // 論理演算(R形式)
    || inst === AND
    || inst === OR
    || inst === XOR

    // 論理演算(I形式)
    || inst === ANDI
    || inst === ORI
    || inst === XORI
  ) {
    // レジスタ wb_data を書き込む
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
