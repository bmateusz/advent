package hu.borcsok.advent.y2017

import org.specs2._

class CaptchaSpecs extends Specification {

  def captcha(n: Int): Int = Captcha.part1(n.toString)

  def captcha2(n: Int): Int = Captcha.part2(n.toString)

  def readResource(resource: String): String = scala.io.Source.fromResource(resource).mkString

  def captcha(resource: String): Int = Captcha.part1(readResource(resource))

  def captcha2(resource: String): Int = Captcha.part2(readResource(resource))

  def is =
    s2"""
  Part I
  The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that
  match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.
  
    ${captcha(1122) mustEqual 3} (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
    ${captcha(1111) mustEqual 4} because each digit (all 1) matches the next.
    ${captcha(1234) mustEqual 0} because no digit matches the next.
    ${captcha(91212129) mustEqual 9} because the only digit that matches the next one is the last digit, 9.
    ${captcha("2017/captcha.txt") mustEqual 1089}

  Part II
  Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list.
  That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
  Fortunately, your list has an even number of elements.

    ${captcha2(1212) mustEqual 6}: the list contains 4 items, and all four digits match the digit 2 items ahead.
    ${captcha2(1221) mustEqual 0}, because every comparison is between a 1 and a 2.
    ${captcha2(123425) mustEqual 4}, because both 2s match each other, but no other digit has a match.
    ${captcha2(123123) mustEqual 12}.
    ${captcha2(12131415) mustEqual 4}.
    ${captcha2("2017/captcha.txt") mustEqual 1156}
  """
}
