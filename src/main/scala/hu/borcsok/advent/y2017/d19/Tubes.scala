package hu.borcsok.advent.y2017.d19

import scala.annotation.tailrec

sealed trait Direction {
  val x: Int
  val y: Int
  val left: Direction
  val right: Direction
}

case object Left extends Direction {
  override val x: Int = -1
  override val y: Int = 0
  override val left: Direction = Down
  override val right: Direction = Up
}

case object Right extends Direction {
  override val x: Int = 1
  override val y: Int = 0
  override val left: Direction = Up
  override val right: Direction = Down
}

case object Up extends Direction {
  override val x: Int = 0
  override val y: Int = -1
  override val left: Direction = Left
  override val right: Direction = Right
}

case object Down extends Direction {
  override val x: Int = 0
  override val y: Int = 1
  override val left: Direction = Right
  override val right: Direction = Left
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

  @tailrec
  def step(input: Seq[String], turtle: Turtle, result: Result = Result()): Result = {

    val newResult: Result = result.step(get(input, turtle))

    Seq(turtle, turtle.left, turtle.right).view.map(_.move).find(t => isValid(input, t)) match {
      case Some(turtleMoved) => step(input, turtleMoved, newResult)
      case None => newResult
    }

  }

  def isValid(input: Seq[String], turtle: Turtle): Boolean = {
    0 <= turtle.y && turtle.y < input.size &&
    0 <= turtle.x && turtle.x < input(turtle.y).length &&
    input(turtle.y)(turtle.x) != ' '
  }

  def get(input: Seq[String], turtle: Turtle): Char = input(turtle.y)(turtle.x)

}
