package hu.borcsok.advent.y2017.d17

case class Spinlock(mem: List[Int] = List(0), p: Int = 0) {

  def steps(n: Int, step: Int): Spinlock = {
    (1 to n).foldLeft(this) {
      (spinlock, i) =>
        val newP = (spinlock.p + step) % i + 1
        Spinlock((spinlock.mem.take(newP) :+ i) ++ spinlock.mem.drop(newP), newP)
    }
  }

  def around(n: Int): List[Int] = {
    val idx = mem.indexOf(n)
    mem.slice(idx - 3, idx + 4)
  }

  def after(n: Int): Int = {
    val idx = mem.indexOf(n)
    mem(idx + 1)
  }

  def stepsAfterZero(n: Int, step: Int): Int = {
    (1 to n).foldLeft((0, 0)) {
      case ((cachedMem: Int, cachedP: Int), i) =>
        val newP: Int = (cachedP + step) % i + 1
        if (newP == 1) (i, newP)
        else (cachedMem, newP)
    }._1
  }

}
