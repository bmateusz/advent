package hu.borcsok.advent.y2017.d01

object Captcha {

  def stringToDigits(str: String): Seq[Int] = str.map((d: Char) => d.asDigit)

  def part1(str: String): Int = run(stringToDigits(str), 1)

  def part2(str: String): Int = run(stringToDigits(str), str.length / 2)

  def run(digits: Seq[Int], step: Int): Int = {
    val rotated: Seq[Int] = digits.drop(step) ++ digits.take(step)
    digits.zip(rotated).map {
      case (a, b) =>
        if (a == b) a
        else 0
    }.sum
  }

}
