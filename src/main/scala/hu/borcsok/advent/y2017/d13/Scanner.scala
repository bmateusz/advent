package hu.borcsok.advent.y2017.d13

import scala.annotation.tailrec

case class Layer(depth: Int, position: Int = 0, direction: Int = 1) {
  def move: Layer = {
    val newPosition = position + direction
    if (newPosition >= 0 && newPosition < depth) {
      Layer(depth, newPosition, direction)
    } else {
      val newDirection = direction * -1
      Layer(depth, position + newDirection, newDirection)
    }
  }
}

case class Scanner(seq: Seq[(Int, Layer)]) {

  def find(int: Int): Option[Layer] = seq.find(_._1 == int).map(_._2)

  def move: Scanner = Scanner(seq.map(x => (x._1, x._2.move)))

  val end: Int = seq.last._1

  def simulate: Seq[Int] = {
    (0 to end).foldLeft((this, Seq.empty[Int])) {
      case ((scan, caught), curr) =>
        val isCaught = scan.find(curr) match {
          case Some(layer) => layer.position == 0
          case None => false
        }
        (scan.move, if (isCaught) caught :+ curr else caught)
    }._2
  }

  def depth(int: Int): Int = find(int) match {
    case Some(value) => value.depth
    case None => 0
  }

  def severity: Int = simulate.map(x => x * depth(x)).sum

  @tailrec
  final def safelyAndSlowly(delay: Int = 1): Int = {
    val step = move
    if (move.simulate == Seq.empty) delay
    else move.safelyAndSlowly(delay + 1)
  }

  def safely: Int = {
    val initial = (0 to end).flatMap {
      n =>
        find(n) match {
          case Some(value) => Some((1 to n).foldLeft(value)((layer, _) => layer.move))
          case None => None
        }
    }
    safelyStep(initial, 0)
  }

  @tailrec
  final def safelyStep(seq: Seq[Layer], delay: Int): Int = {
    if (seq.exists(_.position == 0)) {
      safelyStep(seq.map(_.move), delay + 1)
    } else {
      delay
    }
  }

}

object ScannerHelper {

  def fromStrings(strs: Seq[String]): Scanner = {
    Scanner(
      strs.map {
        str =>
          val splitStr = str.split(": ")
          (splitStr(0).toInt, Layer(splitStr(1).toInt))
      }
    )
  }

}
