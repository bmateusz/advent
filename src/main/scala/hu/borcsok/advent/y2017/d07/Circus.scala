package hu.borcsok.advent.y2017.d07

case class Graph(nodes: Map[String, Int], edges: Map[String, Array[String]]) {

  lazy val roots: Set[String] = nodes.keySet.diff(edges.values.flatten.toSet)

  lazy val weight: Int = weightFrom(roots.head)

  private def weightFrom(node: String): Int = {
    edges(node) match {
      case e: Array[String] if e.length > 0 =>
        val weightOfEdges = e.map(weightFrom)
        if (weightOfEdges.distinct.length > 1) {
          println(s"$node is not balanced ${edges(node).toList} ${weightOfEdges.toList}")
        }
        nodes(node) + weightOfEdges.sum
      case _ => nodes(node)
    }
  }

  def updateWeight(node: String, value: Int): Graph = Graph(nodes.updated(node, value), edges)

}

object Circus {

  def read(lines: Seq[String]): Graph = {
    val rawGraph: Seq[(String, Int, Array[String])] = lines.map {
      line =>
        val nodeData = line.split("\\s+")
        val name = nodeData(0)
        val weight = nodeData(1).replace("(", "").replace(")", "").toInt
        val edges = nodeData.drop(3).map(_.replace(",", ""))

        (name, weight, edges)
    }

    Graph(rawGraph.map(g => g._1 -> g._2).toMap, rawGraph.map(g => (g._1, g._3)).toMap)
  }

}
