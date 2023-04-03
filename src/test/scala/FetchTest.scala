package fetch

import chisel3._
import org.scalatest._
import chiseltest._

class HexTest extends FlatSpec with ChiselScalatestTester {
  "mycpu" should "work through hex" in {
    test(new Top) { c =>
      // for (n <- 0 to 2) {
      //   c.clock.step(1)
      //   if (c.io.exit.peek().litToBoolean) {
      //     print("exit: true\n")
      //   } else {
      //     print("exit: false\n")
      //   }
      // }
      while (!c.io.exit.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }
}