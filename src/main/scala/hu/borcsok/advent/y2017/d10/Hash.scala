package hu.borcsok.advent.y2017.d10

object Hash {

  def hashSimple(seq: Seq[Int], skips: Int = 256): Int = hashStep(seq, skips, 0 until skips)._1.take(2).product

  private def hashStep(seq: Seq[Int], skips: Int, lengths: Seq[Int], startPos: Int = 0, startSkipSize: Int = 0) = {

    val (resultHash, resultPos, resultSkipSize) = seq.foldLeft((lengths, startPos, startSkipSize)) {
      case ((hash: Seq[Int], pos: Int, skipSize: Int), curr: Int) =>
        // println(hash.map(_.toString).updated(pos, s"(${hash(pos)})").mkString(" "))
        val circular = Iterator.continually(hash).flatten
        val newPart = circular.slice(pos, pos + curr).toSeq.reverse
        val newHash = replaceInSeq(hash, newPart, pos)

        (newHash, (pos + curr + skipSize) % skips, (skipSize + 1) % skips)
    }

    (resultHash, resultPos, resultSkipSize)
  }

  private def replaceInSeq(hash: Seq[Int], newPart: Seq[Int], startFrom: Int): Seq[Int] = {
    val begin = hash.take(startFrom) ++ newPart
    val overflow = begin.length - hash.length

    if (overflow > 0) {
      val leftSide = newPart.takeRight(overflow)
      val rightSide = newPart.dropRight(overflow)
      val middle = hash.slice(leftSide.length, hash.length - rightSide.length)
      leftSide ++ middle ++ rightSide
    } else {
      begin ++ hash.drop(begin.length)
    }
  }

  def toAscii(str: String): Seq[Int] = str.map(_.toInt)

  def fromStr(str: String): Seq[Int] = toAscii(str) ++ Seq(17, 31, 73, 47, 23)

  def hashStr(str: String): (Seq[Int], Int, Int) = {
    val input: Seq[Int] = fromStr(str)
    val rounds: Seq[Int] = 1 to 64
    val initialLengths: Seq[Int] = 0 until 256
    rounds.foldLeft((initialLengths, 0, 0)) {
      case ((lengths: Seq[Int], pos: Int, skipSize: Int), curr) =>
        val (newLengths, newPos, newSkipSize) = hashStep(input, 256, lengths, pos, skipSize)
        (newLengths, newPos, newSkipSize)
    }
  }

  def hash64(str: String): String = {
    xorToHex(hashToXor(hashStr(str)._1))
  }

  def hashToXor(seq: Seq[Int]): Seq[Int] = {
    seq.grouped(16).map(_.reduceLeft(_ ^ _)).toSeq
  }

  def xorToHex(seq: Seq[Int]): String = {
    seq.map { (c: Int) =>
      val hex = c.toHexString
      if (hex.length == 2) hex else s"0$hex"
    }.mkString
  }

}
