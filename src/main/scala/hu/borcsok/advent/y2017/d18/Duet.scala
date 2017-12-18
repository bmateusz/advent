package hu.borcsok.advent.y2017.d18

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

class Duet(val mem: Map[String, Long]) {

  val sndBank = "snd"
  val rcvBank = "rcv"
  val sndCount = "sdc"

  def getValOrMem(str: String): Long = {
    Try(str.toLong) match {
      case Success(value) => value
      case Failure(_: NumberFormatException) => getMem(str)
      case Failure(ex) => throw ex
    }
  }

  def getMem(a: String): Long = {
    mem.getOrElse(a, 0L)
  }

  def set(a: String, b: String) = new Duet(mem.updated(a, getValOrMem(b)))

  def add(a: String, b: String) = new Duet(mem.updated(a, getValOrMem(a) + getValOrMem(b)))

  def mul(a: String, b: String) = new Duet(mem.updated(a, getValOrMem(a) * getValOrMem(b)))

  def mod(a: String, b: String) = new Duet(mem.updated(a, getValOrMem(a) % getValOrMem(b)))

  def snd(a: String) = new Duet(mem.updated(sndBank, getValOrMem(a)).updated(sndCount, getMem(sndCount) + 1))

  def rcv1(a: String): Duet = {
    if (getMem(a) != 0) new Duet(mem.updated(rcvBank, getMem(sndBank)))
    else this
  }

  def rcv2(a: String, what: Long): Duet = {
    new Duet(mem.updated(a, what))
  }

  def lastRcv: Long = getMem(rcvBank)
  def lastSnd: Long = getMem(sndBank)
  def countSnd: Long = getMem(sndCount)
}

object Duet {
  def read(lines: Seq[String]): Program = {
    Program(
      lines.map {
        line => line.split(" ")
      }
    )
  }
  def empty = new Duet(Map.empty)
  def id(i: Long) = new Duet(Map("p" -> i))
}

case class Program(pr: Seq[Array[String]]) {
  @tailrec
  final def execute(state: State = State()): Duet = {
    if (isValid(state)) {
      val newState = stepPart1(state)
      if (newState.duet.lastRcv == 0) execute(newState)
      else newState.duet
    } else {
      state.duet
    }
  }

  def isValid(state: State): Boolean = 0 <= state.pc && state.pc < pr.length

  def stepPart1(state: State): State = {
    val duet = state.duet
    val sp = pr(state.pc)
    sp(0) match {
      case "set" => State(duet.set(sp(1), sp(2)), state.pc + 1)
      case "add" => State(duet.add(sp(1), sp(2)), state.pc + 1)
      case "mul" => State(duet.mul(sp(1), sp(2)), state.pc + 1)
      case "mod" => State(duet.mod(sp(1), sp(2)), state.pc + 1)
      case "snd" => State(duet.snd(sp(1)), state.pc + 1)
      case "rcv" => State(duet.rcv1(sp(1)), state.pc + 1)
      case "jgz" =>
        if (duet.getValOrMem(sp(1)) > 0) State(duet, state.pc + duet.getValOrMem(sp(2)).toInt)
        else State(duet, state.pc + 1)
    }
  }

  def stepPart2(state: State, otherState: State): (State, State) = {
    val duet = state.duet
    val sp = pr(state.pc)
    sp(0) match {
      case "set" => (State(duet.set(sp(1), sp(2)), state.pc + 1, state.in), otherState)
      case "add" => (State(duet.add(sp(1), sp(2)), state.pc + 1, state.in), otherState)
      case "mul" => (State(duet.mul(sp(1), sp(2)), state.pc + 1, state.in), otherState)
      case "mod" => (State(duet.mod(sp(1), sp(2)), state.pc + 1, state.in), otherState)
      case "snd" =>
        val snd = duet.snd(sp(1))
        (State(snd, state.pc + 1, state.in), otherState.copy(in = otherState.in.enqueue(snd.lastSnd)))
      case "rcv" =>
        if (state.in.isEmpty) {
          (state.copy(waiting = true), otherState)
        } else {
          val (elem, newIn) = state.in.dequeue
          val rcv = duet.rcv2(sp(1), elem)
          (State(rcv, state.pc + 1, newIn), otherState)
        }
      case "jgz" =>
        if (duet.getValOrMem(sp(1)) > 0) (State(duet, state.pc + duet.getValOrMem(sp(2)).toInt, state.in), otherState)
        else (State(duet, state.pc + 1, state.in), otherState)
    }
  }

  def duet(): Long = {
    val states = Seq(0, 1).map(
      i => State(Duet.id(i))
    )
    duetStep(states(0), states(1))._2.duet.countSnd
  }

  @tailrec
  final def duetStep(states: (State, State)): (State, State) = {
    val tmpStates = stepPart2(states._1, states._2)
    val newStates = stepPart2(tmpStates._2, tmpStates._1).swap
    if (newStates._1.waiting && newStates._2.waiting) states
    else {
      duetStep(newStates._1, newStates._2)
    }
  }
}

case class State(duet: Duet = Duet.empty,
                 pc: Int = 0,
                 in: Queue[Long] = Queue.empty,
                 waiting: Boolean = false)
