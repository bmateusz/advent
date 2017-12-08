package hu.borcsok.advent.y2017.d02

import org.specs2.Specification
import hu.borcsok.advent.y2017.Helpers._

class ChecksumSpecs extends Specification {

  def checkRow(str: String): Int = Checksum.minMaxDiff(lineToInts(str))

  def checkResource(str: String): Int = Checksum.minMaxSum(readResourceLines(str).map(lineToInts))

  def checkDiv(str: String): Int = Checksum.divides(lineToInts(str))

  def checkDivResource(str: String): Int = Checksum.dividesSum(readResourceLines(str).map(lineToInts))

  def is =
s2"""
# Day 02

## Part I
The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track,
they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value
and the smallest value; the checksum is the sum of all of these differences.

For example, given the following spreadsheet:

${checkRow("5 1 9 5") === 8}
${checkRow("7 5 3") === 4}
${checkRow("2 4 6 8") === 6}
The first row's largest and smallest values are 9 and 1, and their difference is 8.
The second row's largest and smallest values are 7 and 3, and their difference is 4.
The third row's difference is 6.
In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

${checkResource("2017/checksum.txt") === 45351}

Part 2
It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is,
where the result of the division operation is a whole number. They would like you to find those numbers on each line,
divide them, and add up each line's result.

For example, given the following spreadsheet:

${checkDiv("5 9 2 8") === 4}
${checkDiv("9 4 7 3") === 3}
${checkDiv("3 8 6 5") === 2}
In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
In the second row, the two numbers are 9 and 3; the result is 3.
In the third row, the result is 2.
In this example, the sum of the results would be 4 + 3 + 2 = 9.

${checkDivResource("2017/checksum.txt") === 275}
"""
}
