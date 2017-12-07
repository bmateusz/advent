package hu.borcsok.advent.y2017.d04

object Passphrase {

  def fromString(str: String): Seq[String] = str.split("\\s+").toSeq

  def isUnique(keys: Seq[String]): Boolean = keys.distinct.size == keys.size

  def sort(seq: Seq[String]): Seq[String] = seq.map(_.sorted)

}
