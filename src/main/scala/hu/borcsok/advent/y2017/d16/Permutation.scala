package hu.borcsok.advent.y2017.d16

sealed trait Command

case class Spin(p: Int) extends Command

case class Exchange(a: Int, b: Int) extends Command

case class Partner(a: Char, b: Char) extends Command

object Permutation {

  def run(str: String, n: Int): String = {
    runPr(commands(str), initial(n)).mkString
  }

  def runPr(cmds: Vector[Command], mem: Vector[Char]): Vector[Char] = {
    cmds.foldLeft(mem) {
      (mem, pr) =>
        execute(mem, pr)
    }
  }

  def commands(str: String): Vector[Command] = {
    str.split(",").map {
      pr =>
        pr(0) match {
          case 's' =>
            Spin(pr.substring(1).toInt)
          case 'x' =>
            val ex = pr.substring(1).split("/")
            Exchange(ex(0).toInt, ex(1).toInt)
          case 'p' =>
            Partner(pr(1), pr(3))
        }
    }.toVector
  }

  def execute(chars: Vector[Char], command: Command): Vector[Char] = {
    command match {
      case Spin(p) => chars.takeRight(p) ++ chars.dropRight(p)
      case Exchange(a, b) => chars.updated(a, chars(b)).updated(b, chars(a))
      case Partner(a, b) =>
        val (pa, pb) = (chars.indexOf(a), chars.indexOf(b))
        chars.updated(pa, chars(pb)).updated(pb, chars(pa))
    }
  }

  def initial(n: Int): Vector[Char] = (0 until n).map(i => (i + 'a').toChar).toVector

  def charToInt(c: Char): Int = {
    val ret = c - 'a'
    if (0 <= ret && ret <= 15) ret
    else throw new Exception(s"'$c' is not valid")
  }

  val billion = 1000000000

  def stress(str: String, n: Int): String = {
    (1 to billion % 36).foldLeft(initial(n)) {
      (mem, i) =>
        val ret = runPr(commands(str), mem)
        // println(s"$i $ret")
        if (ret == initial(n)) throw new Exception(s"cut $i")
        ret
    }.mkString
  }
}
