package hu.borcsok.advent.y2017

object Captcha {

  def part1(digits: String): Int = run(digits.map((d: Char) => d.asDigit), 1)

  def part2(digits: String): Int = run(digits.map((d: Char) => d.asDigit), digits.length / 2)

  def run(digits: Seq[Int], step: Int): Int = {
    val rotated: Seq[Int] = digits.drop(step) ++ digits.take(step)
    digits.zip(rotated).map {
      case (a, b) =>
        if (a == b) a
        else 0
    }.sum
  }

}
