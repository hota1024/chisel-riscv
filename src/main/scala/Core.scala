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

  // inst が 0x34333231(プログラムの最終行) なら終了
  io.exit := (inst === 0x34333231.U(WORD_LEN.W))

  /*-------------------------*/
  /*          Debu           */
  printf(p"pc_reg : 0x${Hexadecimal(pc_reg)}\n")
  printf(p"inst   : 0x${Hexadecimal(inst)}\n")
  printf("-----------------------------\n")
}
