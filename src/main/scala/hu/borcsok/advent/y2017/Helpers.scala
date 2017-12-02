package hu.borcsok.advent.y2017

object Helpers {

  def readResource(resource: String): String = scala.io.Source.fromResource(resource).mkString

  def readResourceLines(resource: String): Seq[String] = scala.io.Source.fromResource(resource).getLines.toSeq

  def lineToInts(str: String): Seq[Int] = str.split("\\s").map(_.toInt)

}
