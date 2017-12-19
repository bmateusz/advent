package hu.borcsok.advent.y2017.d19

sealed trait Direction {
  val x: Int
  val y: Int
  def left: Direction
  def right: Direction
}

case object Left extends Direction {
  override val x: Int = -1
  override val y: Int = 0
  override def left: Direction = Down
  override def right: Direction = Up
}

case object Right extends Direction {
  override val x: Int = 1
  override val y: Int = 0
  override def left: Direction = Up
  override def right: Direction = Down
}

case object Up extends Direction {
  override val x: Int = 0
  override val y: Int = -1
  override def left: Direction = Left
  override def right: Direction = Right
}

case object Down extends Direction {
  override val x: Int = 0
  override val y: Int = 1
  override def left: Direction = Right
  override def right: Direction = Left
}

case class Turtle(x: Int, y: Int, direction: Direction) {
  def move = Turtle(x + direction.x, y + direction.y, direction)
  def left = Turtle(x, y, direction.left)
  def right = Turtle(x, y, direction.right)
}

case class Result(text: String = "", steps: Int = 0) {
  def step(curr: Char) = Result(
    if (Seq('|', '-', '+').contains(curr)) text else text + curr,
    steps + 1
  )
}

object Tubes {

  def walk(input: Seq[String]): Result = {
    step(input, Turtle(input.head.indexOf('|'), 0, Down))
  }

  def step(input: Seq[String], turtle: Turtle, result: Result = Result()): Result = {

    val newResult: Result = result.step(get(input, turtle))

    val next: Turtle = {
      val tryStraight = turtle.move
      if (isValid(input, tryStraight)) {
        tryStraight
      } else {
        val tryLeft = turtle.left.move
        if (isValid(input, tryLeft)) {
          tryLeft
        } else {
          val tryRight = turtle.right.move
          if (isValid(input, tryRight)) {
            tryRight
          } else {
            turtle
          }
        }
      }
    }

    if (next == turtle) {
      newResult
    } else {
      step(input, next, newResult)
    }
  }

  def isValid(input: Seq[String], turtle: Turtle): Boolean = {
    0 <= turtle.y && turtle.y < input.size &&
    0 <= turtle.x && turtle.x < input(turtle.y).length &&
    input(turtle.y)(turtle.x) != ' '
  }

  def get(input: Seq[String], turtle: Turtle): Char = input(turtle.y)(turtle.x)

}
