package hu.borcsok.advent.y2017.d15

case class Generator(a: Long, b: Long, critA: Long = 1, critB: Long = 1) {

  val factorA = 16807
  val factorB = 48271
  val modulo = 2147483647
  val mask = 65535

  def step = Generator(
    stepUntilModulo(a, factorA, critA),
    stepUntilModulo(b, factorB, critB),
    critA,
    critB
  )

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

  def stepUntilModulo(x: Long, factor: Long, crit: Long): Long = {
    val candidate = x * factor % modulo
    if (candidate % crit == 0) candidate
    else stepUntilModulo(candidate, factor, crit)
  }

  def toPicky = Generator(a, b, 4, 8)

}
