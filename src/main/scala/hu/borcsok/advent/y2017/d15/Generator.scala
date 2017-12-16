package hu.borcsok.advent.y2017.d15

import scala.annotation.tailrec

sealed trait Generator {

  val a: Long
  val b: Long

  def step: Generator

  val factorA = 16807
  val factorB = 48271
  val modulo = 2147483647
  val mask = 65535

  def run(i: Int): Generator = (1 to i).foldLeft(this) {
    (gen, _) => gen.step
  }

  def judge(i: Int): Int = (1 to i).foldLeft((this, 0)) {
    case ((gen, points), _) =>
      val newGen = gen.step
      (newGen, points + (if (newGen.isLowest16Match) 1 else 0))
  }._2

  def isLowest16Match: Boolean = {
    (a & mask) == (b & mask)
  }
}

case class SimpleGenerator(a: Long, b: Long) extends Generator {

  def step = SimpleGenerator(
    a * factorA % modulo,
    b * factorB % modulo,
  )

  def toPicky: PickyGenerator = PickyGenerator(a, b)

}

case class PickyGenerator(a: Long, b: Long) extends Generator {

  def step = PickyGenerator(
    stepUntilModulo(a, factorA, 4),
    stepUntilModulo(b, factorB, 8),
  )

  @tailrec
  final def stepUntilModulo(x: Long, factor: Long, criteria: Long): Long = {
    val candidate = x * factor % modulo
    if (candidate % criteria == 0) candidate
    else stepUntilModulo(candidate, factor, criteria)
  }

}
