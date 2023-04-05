package fetch

import chisel3._
import chisel3.util._
import common.Consts._

class Core extends Module {
  val io = IO(new Bundle {
    // メモリの入出力
    val imem = Flipped(new ImemPortIo())

    // 処理が終わると true.B になる出力
    val exit = Output(Bool())
  })

  // 32本の32bit幅(WORD_LEN.W)レジスタを用意
  val regfile = Mem(32, UInt(WORD_LEN.W))

  /*-------------------------*/
  /* Instruction Fetch Stage */

  // PC レジスタ
  val pc_reg = RegInit(START_ADDR)

  // 立ち上がりエッジで pc_reg += 4 する回路
  pc_reg := pc_reg + 4.U(WORD_LEN.W)

  // 取得する命令のアドレスを PC で設定
  io.imem.addr := pc_reg

  // 命令を取得
  val inst = io.imem.inst

  val rs1_addr = inst(19, 15) // rs1
  val rs2_addr = inst(24, 20) // rs2
  val wb_addr  = inst(11, 7)  // rd

  /**
    * RISC-V では0番レジスタの値は常に0になる。
    */

  // rs1 のデータを取得(無効なアドレスなら0.U)
  val rs1_data = Mux((rs1_addr =/= 0.U(WORD_LEN.U)), regfile(rs1_addr), 0.U(WORD_LEN.W))

  // rs2 のデータを取得(無効なアドレスなら0.U)
  val rs2_data = Mux((rs2_addr =/= 0.U(WORD_LEN.U)), regfile(rs2_addr), 0.U(WORD_LEN.W))

  /*-------------------------*/
  /*          Debug          */
  printf(p"pc_reg   : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst     : 0x${Hexadecimal(inst)}\n")
  printf(p"rs1_addr : $rs1_addr\n")
  printf(p"rs2_addr : $rs2_addr\n")
  printf(p"wb_addr  : $wb_addr\n")
  printf(p"rs1_data : 0x${Hexadecimal(rs1_data)}\n")
  printf(p"rs2_data : 0x${Hexadecimal(rs2_data)}\n")
  printf("-----------------------------\n")

  // inst が 0x44434241(プログラムの最終行) なら終了
  io.exit := (inst === 0x44434241.U(WORD_LEN.W))
}
