package hu.borcsok.advent.y2017.d20

import scala.annotation.tailrec
import scala.util.matching.Regex

case class Triple(x: Int, y: Int, z: Int) {
  lazy val manhattan: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  def +(other: Triple) = Triple(x + other.x, y + other.y, z + other.z)
}

case class Particle(position: Triple, velocity: Triple, acceleration: Triple) {
  lazy val step: Particle = {
    val newVelocity = velocity + acceleration
    val newPos = position + newVelocity
    Particle(newPos, newVelocity, acceleration)
  }
}

object Particles {

  val row: Regex = raw"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>".r

  def read(seq: Seq[String]): Particles = {
    new Particles(seq.map {
      case row(p1, p2, p3, v1, v2, v3, a1, a2, a3) =>
        Particle(
          Triple(p1.toInt, p2.toInt, p3.toInt),
          Triple(v1.toInt, v2.toInt, v3.toInt),
          Triple(a1.toInt, a2.toInt, a3.toInt)
        )
    })
  }
}

class Particles(val particles: Seq[Particle]) {

  def closestInLongRun: Int = {
    val slowestAccelerationSpeed = particles.map(_.acceleration.manhattan).min
    val slowestAcceleration = particles.zipWithIndex.filter(_._1.acceleration.manhattan == slowestAccelerationSpeed)
    val closestPosition = slowestAcceleration.minBy(_._1.position.manhattan)
    closestPosition._2
  }

  @tailrec
  final def simulate(n: Int, particlesLost: Int = 0): Int = {
    if (n > 0) {
      val (newParticles, newCollisions) = this.step
      newParticles.simulate(n - 1, particlesLost + newCollisions)
    }
    else particlesLost
  }

  def step: (Particles, Int) = {
    val (rest, collisions) = particles.groupBy(_.position).partition(_._2.lengthCompare(1) == 0)
    (
      new Particles(rest.values.flatten.toSeq.map(_.step)),
      collisions.values.flatten.size
    )
  }
}
