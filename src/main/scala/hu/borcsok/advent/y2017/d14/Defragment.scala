package hu.borcsok.advent.y2017.d14

import hu.borcsok.advent.y2017.d10.Hash

import scala.annotation.tailrec

object Defragment {

  def hash(str: String): Seq[String] = {
    (0 to 127).par.
      map(n => Hash.hashBin(s"$str-$n")).seq
  }

  @tailrec
  def regions(seq: Seq[String], n: Int = 0): Int = {
    seq.headOption match {
      case Some(head) => head.zipWithIndex.find(_._1 == '1') match {
        case Some((_: Char, x: Int)) =>
          regions(erase(seq, x, 0), n + 1)
        case None => regions(seq.tail, n)
      }
      case None => n
    }
  }

  val moves = Seq((-1, 0), (0, -1), (1, 0), (0, 1))

  def erase(seq: Seq[String], x: Int, y: Int): Seq[String] = {
    if (0 <= y && y < seq.length && 0 <= x && x < seq(y).length && seq(y)(x) == '1') {
      val newSeq = seq.updated(y, seq(y).updated(x, '0'))
      moves.foldLeft(newSeq) {
        case (currSeq, (dx, dy)) => erase(currSeq, x + dx, y + dy)
      }
    } else {
      seq
    }
  }
}
