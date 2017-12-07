package hu.borcsok.advent.y2017.d06

import scala.annotation.tailrec

case class RedistributionResult(cycles: Int, seenAgain: Int)

case class Redistribution(mem: Vector[Int], seen: Map[Vector[Int], Int] = Map.empty) {

  val (maxValue, placeOfMax) = mem.zipWithIndex.maxBy(_._1)

  val step: Vector[Int] = doStep(maxValue, safeIncrement(placeOfMax, mem.length), mem.updated(placeOfMax, 0))

  @tailrec
  private def doStep(value: Int, at: Int, memStep: Vector[Int]): Vector[Int] =
    if (value <= 0) memStep
    else doStep(
      value - 1,
      safeIncrement(at, memStep.length),
      memStep.updated(at, memStep(at) + 1)
    )

  @tailrec
  final def findLoop(n: Int = 1): RedistributionResult =
    seen.get(step) match {
      case Some(firstSeenInCycle) => RedistributionResult(n, n - firstSeenInCycle)
      case None => Redistribution(step, seen + (step -> n)).findLoop(n + 1)
    }

  private def safeIncrement(at: Int, length: Int) = if (at >= length - 1) 0 else at + 1

}
