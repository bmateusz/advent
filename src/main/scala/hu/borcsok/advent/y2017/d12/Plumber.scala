package hu.borcsok.advent.y2017.d12

case class Edge(from: Int, to: Int)

case class Edges(seq: Seq[Edge]) {

  def reach(what: Int, marked: Set[Int] = Set.empty): Set[Int] = seq.filter(_.from == what).map {
    value =>
      if (marked.contains(value.to)) marked
      else reach(value.to, marked + value.to)
  }.reduce(_ ++ _)

  // 5s 179 ms
  def groupsSlow: Set[Set[Int]] = {
    seq.map(_.from).toSet.map((e: Int) => reach(e))
  }

  // 440 ms
  def groups: Set[Set[Int]] = {
    seq.map(_.from).foldLeft(Set.empty[Set[Int]]) {
      (result: Set[Set[Int]], curr: Int) =>
        if (result.exists(_.contains(curr))) result
        else result + reach(curr)
    }
  }

}

object Plumber {

  def createEdges(seq: Seq[String]): Edges = {
    Edges(seq.flatMap {
      line =>
        val splits = line.split(" <-> ")
        val left: String = splits(0)
        val right: String = splits(1)
        right.split(", ").map {
          (oneRight: String) => Edge(left.toInt, oneRight.toInt)
        }
    })
  }

}
