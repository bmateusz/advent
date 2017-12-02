package hu.borcsok.advent.y2017.d02

object Checksum {

  def minMaxDiff(line: Seq[Int]): Int = {
    line.max - line.min
  }

  def minMaxSum(matrix: Seq[Seq[Int]]): Int = {
    matrix.map(minMaxDiff).sum
  }

  def searchDiv(numbers: Seq[Int]): Int = {
    numbers.toList match {
      case x :: xs =>
        xs.find {
          n => x % n == 0
        } match {
          case Some(value) => x / value
          case None => searchDiv(xs)
        }

      case Nil => 0
    }
  }

  def divides(line: Seq[Int]): Int = {
    searchDiv(line.sortWith(_ > _))
  }

  def dividesSum(matrix: Seq[Seq[Int]]): Int = {
    matrix.map(divides).sum
  }

}
