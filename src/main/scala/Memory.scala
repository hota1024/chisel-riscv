package fetch

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import common.Consts._

// 命令用メモリポート
class ImemPortIo extends Bundle {
  // アドレス
  val addr = Input(UInt(WORD_LEN.W))

  // addr が示す命令列
  val inst = Output(UInt(WORD_LEN.W))
}

// データ用メモリポート
class DmemPortIo extends Bundle {
  // アドレス
  val addr  = Input(UInt(WORD_LEN.W))
  // addr が示すデータ
  val rdata = Output(UInt(WORD_LEN.W))

  // 書き込みのタイミングを表す可否信号
  val wen   = Input(Bool())
  // 書き込むデータ
  val wdata = Input(UInt(WORD_LEN.W))
}

class Memory extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
    val dmem = new DmemPortIo()
  })

  // メモリの実態
  val mem = Mem(16384, UInt(8.W))

  // メモリデータのロード
  loadMemoryFromFile(mem, "/src/src/hex/br_hazard.hex")

  // 命令用メモリの inst に addr が示す値を接続
  io.imem.inst := Cat(
    mem(io.imem.addr + 3.U(WORD_LEN.W)),
    mem(io.imem.addr + 2.U(WORD_LEN.W)),
    mem(io.imem.addr + 1.U(WORD_LEN.W)),
    mem(io.imem.addr)
  )

  // データ用メモリの rdata に addr が示す値を接続
  io.dmem.rdata := Cat(
    mem(io.dmem.addr + 3.U(WORD_LEN.W)),
    mem(io.dmem.addr + 2.U(WORD_LEN.W)),
    mem(io.dmem.addr + 1.U(WORD_LEN.W)),
    mem(io.dmem.addr)
  )

  when(io.dmem.wen) {
    // 書き込み信号がONのときの処理

    mem(io.dmem.addr)       := io.dmem.wdata( 7,  0)
    mem(io.dmem.addr + 1.U) := io.dmem.wdata(15,  8)
    mem(io.dmem.addr + 2.U) := io.dmem.wdata(23, 16)
    mem(io.dmem.addr + 3.U) := io.dmem.wdata(31, 24)
  }
}
