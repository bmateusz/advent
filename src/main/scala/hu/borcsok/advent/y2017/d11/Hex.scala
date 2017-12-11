package hu.borcsok.advent.y2017.d11

case class Position(x: Int, y: Int) {
  def +(value: Position): Position = Position(x + value.x, y + value.y)
  def distance: Int = {
    val ax = Math.abs(x)
    val ay = (Math.abs(y) + 1) / 2
    val diagonal = ax min ay
    val rest = (ax max ay) - diagonal
    diagonal + rest
  }
}

object Hex {

/*
    \ n  /
  nw +--+ ne
    /    \
  -+      +-
    \    /
  sw +--+ se
    / s  \
*/

  def move(dir: String): Position = dir match {
    case "n" => Position(0, 2)
    case "ne" => Position(1, 1)
    case "se" => Position(1, -1)
    case "s" => Position(0, -2)
    case "sw" => Position(-1, -1)
    case "nw" => Position(-1, 1)
  }

  def moves(str: String): Seq[String] = str.split(",")

  def finalPosition(str: String): Position = moves(str).map(move).reduce(_ + _)

  def distance(str: String): Int = {
    finalPosition(str).distance
  }

  def furthestDistance(str: String): Int = {
    val (maxDistance, finalPos) = moves(str).map(move).foldLeft((0, Position(0, 0))) {
      case ((maxDistance, pos), curr) =>
        val newPos = pos + curr
        (maxDistance max newPos.distance, newPos)
    }
    maxDistance
  }

}
