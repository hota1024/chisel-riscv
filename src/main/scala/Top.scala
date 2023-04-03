package fetch

import chisel3._
import chisel3.util._

// コンピュータ全体を表すクラス
class Top extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())
  })

  // コアのハードウェア
  val core   = Module(new Core())
  // メモリのハードウェア
  val memory = Module(new Memory())

  // メモリの入出力をコアのメモリに接続
  core.io.imem <> memory.io.imem

  // コアの終了フラグを出力の終了フラグに接続
  io.exit := core.io.exit
}
