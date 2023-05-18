package vsetvli

import chisel3._
import chisel3.util._
import common.Consts

// コンピュータ全体を表すクラス
class Top extends Module {
  val io = IO(new Bundle {
    val exit = Output(Bool())

    // パス条件判定用信号
    val gp = Output(UInt(Consts.WORD_LEN.W))
  })

  // コアのハードウェア
  val core   = Module(new Core())
  // メモリのハードウェア
  val memory = Module(new Memory())

  /* メモリの入出力をコアのメモリに接続 */

  core.io.imem <> memory.io.imem
  core.io.dmem <> memory.io.dmem
  
  // コアの終了フラグを出力の終了フラグに接続
  io.exit := core.io.exit
  io.gp   := core.io.gp
}
