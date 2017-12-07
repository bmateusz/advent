package hu.borcsok.advent.y2017.d05

import scala.annotation.tailrec

sealed trait StepMode {
  def step(from: Int): Int
}

case object StepByOne extends StepMode {
  override def step(from: Int): Int = from + 1
}

case object StepStrange extends StepMode {
  override def step(from: Int): Int = from + (if (from >= 3) -1 else 1)
}

case class Jump(mem: Vector[Int], pointer: Int = 0) {

  def step(stepMode: StepMode): Jump = {
    val curr = mem(pointer)
    Jump(mem.updated(pointer, stepMode.step(mem(pointer))), pointer + curr)
  }

  @tailrec
  final def stepsUntilOutOfBounds(stepMode: StepMode, result: Int = 0): Int = {
    if (pointer < 0 || pointer > mem.length - 1) result
    else step(stepMode).stepsUntilOutOfBounds(stepMode, result + 1)
  }

}
