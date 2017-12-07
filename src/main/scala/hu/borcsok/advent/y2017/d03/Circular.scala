package hu.borcsok.advent.y2017.d03

import scala.annotation.tailrec

object Circular {

  val maxN = 1024

  def manhattan(n: Int): Int = {
    // val nthCircle = whichCircle(n)
    // allCross(nthCircle).map(x => Math.abs(x - n)).min + nthCircle - 1
    positionFrom(n).manhattan
  }

  def pow2(x: Int): Int = x * x

  def circleDownRight(n: Int): Int = pow2(2 * n - 1)

  // TODO get rid of cycle
  def whichCircle(n: Int): Int = (1 to maxN).find(i => n <= circleDownRight(i)) match {
    case Some(nthCircle) => nthCircle
    case None => throw new Exception(s"maxN = $maxN is too small")
  }

  case class Position(x: Int, y: Int) {
    lazy val manhattan: Int = Math.abs(x) + Math.abs(y)
    def add(value: Position): Position = Position(x + value.x, y + value.y)
  }

  def positionFrom(n: Int): Position = {
    val nthCircle = whichCircle(n)
    val crosses = allCross(nthCircle)
    val crossesDiffs = crosses.map(x => x - n)
    val side = crossesDiffs.indexWhere(i => Math.abs(i) == crossesDiffs.map(Math.abs).min)
    val a = nthCircle - 1
    val b = if (side >= 0) crossesDiffs(side) else 0
    side match {
      case -1 => Position(0, 0)
      case 0 => Position(a, b)
      case 1 => Position(b, -a)
      case 2 => Position(-a, -b)
      case 3 => Position(-b, a)
    }
  }

  def pos(n: Int, direction: Int): Int = circleDownRight(n) - direction * (n - 1)

  def allCross(x: Int): Seq[Int] = (1 to 7).by(2).reverse.map(i => pos(x, i))

  def allCorner(x: Int): Seq[Int] = (0 to 6).by(2).reverse.map(i => pos(x, i))

  def isCorner(n: Int): Boolean = allCorner(whichCircle(n)).contains(n)

  def stress(n: Int): Int = {
    val cache = (1 to n).map(i => positionFrom(i) -> (i - 1)).toMap
    StressRunner(n, cache).rec(Seq.empty)
  }

  def stressFirstLargerThan(n: Int): Option[Int] = {
    (1 to maxN).find(i => n < stress(i))
  }

  case class StressRunner(max: Int, cache: Map[Position, Int]) {

    @tailrec
    final def rec(seq: Seq[Int]): Int = {
      val n = seq.length + 1
      if (n > max) seq.last
      else {
        val position = positionFrom(n) // update position cache
        val nextVal =
          if (n < 3) 1
          else sumAroundPosition(position, seq)
        rec(seq :+ nextVal)
      }
    }

    val around = Seq(
      Position(1, 0),
      Position(1, -1),
      Position(0, -1),
      Position(-1, -1),
      Position(-1, 0),
      Position(-1, 1),
      Position(0, 1),
      Position(1, 1)
    )

    def sumAroundPosition(position: Position, seq: Seq[Int]): Int = {
      around.map(position.add).map {
        p =>
          val n = findInCache(p)
          if (n == -1) 0
          else if (n >= seq.length) 0
          else seq(n)
      }.sum
    }

    def findInCache(p: Position): Int = cache.getOrElse(p, -1)

  }
}
