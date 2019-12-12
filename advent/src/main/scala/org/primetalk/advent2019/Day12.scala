package org.primetalk.advent2019

import java.util

import org.primetalk.advent.tools.Geom3dUtils._
import org.primetalk.advent.tools.{PrimeNumbers, SequenceUtils}

import scala.annotation.tailrec

object ImmutableMoon {

  case class MoonState(pos: Position, vel: Vector3d = (0, 0, 0)) {
    def pot: Long = math.abs(pos._1) + math.abs(pos._2) + math.abs(pos._3)

    def kin: Long = math.abs(vel._1) + math.abs(vel._2) + math.abs(vel._3)

    def total: Long = pot * kin

    def gravityFrom(other: MoonState): Vector3d = {
      val d = other.pos - pos
      normalizeDirVector(d)
    }

    def applyGravity(totalGravity: Vector3d): MoonState = copy(vel = vel + totalGravity)

    def move: MoonState = copy(pos = pos + vel)
  }

  case class SimulationState(time: Long, moons: IndexedSeq[MoonState])

  def overallEnergy(moons: Seq[MoonState]): Long = moons.map(_.total).sum

  val initialMoons: IndexedSeq[MoonState] = IndexedSeq(
    MoonState((-10, -10, -13)),
    MoonState((5, 5, -9)),
    MoonState((3, 8, -16)),
    MoonState((1, 3, -3))
  )

  val initialState = SimulationState(0, initialMoons)

  val count: Int = initialMoons.size

  val indexes: IndexedSeq[(Int, List[Int])] =
    for {
      i <- 0 until count
    } yield (i, (for {
      j <- 0 until count
      if i != j
    } yield j).toList)


  def showVector(v: Vector3d): String =
    s"<x=${v._1}, y=${v._2}, z=${v._3}>"

  def show: SimulationState => String = {
    case SimulationState(time, moons) =>
      s"After $time steps:\n" +
        moons.map { case MoonState(pos, vel) => s"pos=${showVector(pos)},\tvel=${showVector(vel)}\n" }.mkString
  }

  def timeStep: SimulationState => SimulationState = {
    case SimulationState(time, moons) =>

      val moonsWithUpdatedVels = for {
        (i, js) <- indexes
        thisMoon = moons(i)
        otherMoons = js.map(j => moons(j))
        gravities = otherMoons.map(other => thisMoon.gravityFrom(other))
        overallGravity = gravities.reduce(_ + _)
        nextVel = thisMoon.vel + overallGravity
      } yield thisMoon.copy(vel = nextVel)

      SimulationState(time + 1, moonsWithUpdatedVels.map(_.move))
  }

  @tailrec
  final def runN(n: Int)(s: SimulationState): SimulationState =
    if (n == 0)
      s
    else {
      //      println(show(s))
      runN(n - 1)(timeStep(s))
    }

  lazy val answer1: Long = {
    val state = initialState
    println(show(state))
    val finalMoons = runN(1000)(state)
    println(show(finalMoons))
    overallEnergy(finalMoons.moons)
  }
}

final class MutableMoon(name: String, initialMoons: IndexedSeq[ImmutableMoon.MoonState]) {

  type MoonStates = Array[Int]

  type MoonStateShift = Int

  val pos = 0
  val vel: Int = pos + 3
//  val nextVel: Int = vel + 3
  val gravity: Int = vel + 3

  val moonSize: Int = gravity + 3

  val _1 = 0
  val _2 = 1
  val _3 = 2

  val count: Int = initialMoons.size

  val moonShifts: IndexedSeq[MoonStateShift] = for{
    i <- 0 until count
    m = i * moonSize
  } yield m

  private val mem: MoonStates = {
    val res = Array.fill[Int](count * moonSize)(0)
    for{
      i <- 0 until count
      p = initialMoons(i).pos
      m = i * moonSize
    } {
      res(m + pos + _1) = p._1
      res(m + pos + _2) = p._2
      res(m + pos + _3) = p._3
    }
    res
  }
  private var time: Long = 0

  def pot(m: MoonStateShift): Int =
    math.abs(mem(m + pos + _1)) +
      math.abs(mem(m + pos + _2)) +
      math.abs(mem(m + pos + _3))

  def kin(m: MoonStateShift): Int =
    math.abs(mem(m + vel + _1)) +
      math.abs(mem(m + vel + _2)) +
      math.abs(mem(m + vel + _3))

  def total(m: MoonStateShift): Int = pot(m: MoonStateShift) * kin(m: MoonStateShift)

  def overallEnergy: Long = moonShifts.map(total).sum

  @inline
  def setZero3d(m: MoonStateShift, rel: Int): Unit = {
    mem(m + rel + _1) = 0
    mem(m + rel + _2) = 0
    mem(m + rel + _3) = 0
  }

  @inline
  def addGravityFrom(m: MoonStateShift, other: MoonStateShift): Unit = {
    @inline
    def normalizedDiff(shift1: Int, shift2: Int): Int =
      Integer.compare(mem(shift1), mem(shift2))
    mem(m + gravity + _1) += -normalizedDiff(m + pos + _1, other + pos + _1)
    mem(m + gravity + _2) += -normalizedDiff(m + pos + _2, other + pos + _2)
    mem(m + gravity + _3) += -normalizedDiff(m + pos + _3, other + pos + _3)
  }

  @inline
  def applyGravity(m: MoonStateShift): Unit = {
    mem(m + vel + _1) += mem(m + gravity + _1)
    mem(m + vel + _2) += mem(m + gravity + _2)
    mem(m + vel + _3) += mem(m + gravity + _3)
  }

  @inline
  def move(m: MoonStateShift): Unit = {
    mem(m + pos + _1) += mem(m + vel + _1)
    mem(m + pos + _2) += mem(m + vel + _2)
    mem(m + pos + _3) += mem(m + vel + _3)
  }


//  def overallEnergy(moons: Seq[MoonState]): Long = moons.map(_.total).sum

  val indexes: IndexedSeq[(Int, List[Int])] =
    for {
      i <- 0 until count
    } yield (i, (for {
      j <- 0 until count
      if i != j
    } yield j).toList)


  def showVector(shift: MoonStateShift): String =
    s"<x=${mem(shift + _1)}, y=${mem(shift + _2)}, z=${mem(shift + _3)}>"

  def showMoon(shift: MoonStateShift): String =
    s"pos=${showVector(shift + pos)},\tvel=${showVector(shift + vel)}\n"

  private val startTimeMs = System.currentTimeMillis()

  def elapsed: String = {
    val elapsedSec = (System.currentTimeMillis() - startTimeMs)/1000
    val elapsedMin = elapsedSec / 60
    val elapsedHours = elapsedMin / 60
    (if(elapsedHours > 0) s"${elapsedHours % 60}h" else "") +
      (if(elapsedMin > 0) s"${elapsedMin % 60}m" else "") +
      s"${elapsedSec % 60}s"
  }

  def showState: String = {

    s"$name: After $time steps (elapsed = $elapsed):\n" +
        moonShifts.map { m => showMoon(m) }.mkString
  }
  @tailrec
  def applyGravityToEachFromAllOthers(i: Int = 0): Unit = if(i < count){
    val m = i * moonSize
    setZero3d(m, gravity)
    @tailrec
    def updateGravityFromOthers(j: Int): Unit = if(j < count){
      if(j != i){
        val other = j * moonSize
        addGravityFrom(m, other)
      }
      updateGravityFromOthers(j + 1)
    } // else {}
    updateGravityFromOthers(0)
    applyGravity(m)
    setZero3d(m, gravity)
    applyGravityToEachFromAllOthers(i + 1)
  } // else {}
  @tailrec
  def moveAll(i: Int = 0): Unit = if(i < count){
    val m = i * moonSize
    move(m)
    moveAll(i + 1)
  } // else {}

  def timeStep(): Unit = {
    applyGravityToEachFromAllOthers()
    moveAll()
    time += 1
//    if(time == 1000000) {
//      println(showState)
//    }
//    if(time == 10000000) {
//      println(showState)
//    }
    if(time % 100000000 == 0) {
      println(showState)
    }
  }


  @tailrec
  def runN(n: Int): Unit =
    if (n > 0) {
      timeStep()
      runN(n - 1)
    }
}

object MutableMoon {
  @inline
  def copy(from: MutableMoon, to: MutableMoon): Unit = {
    Array.copy(from.mem, 0, to.mem, 0, from.moonSize*from.count)
  }
  @inline
  def eq(from: MutableMoon, to: MutableMoon): Boolean =
    util.Arrays.equals(from.mem, to.mem)
  @inline
  def eq(axis: Int)(from: MutableMoon, to: MutableMoon): Boolean = {
    def loop(j: Int): Boolean  = {
      val m = j * from.moonSize
      j == from.count ||
        ((
        from.mem(m + from.pos + axis) == to.mem(m + to.pos + axis) &&
          from.mem(m + from.vel + axis) == to.mem(m + to.vel + axis)
        ) && loop(j + 1)
          )
    }

    loop(0)
  }
}
/**
  *
  * https://adventofcode.com/2019/day/12
  */
object Day12 {

  /*
<x=-10, y=-10, z=-13>
<x=5, y=5, z=-9>
<x=3, y=8, z=-16>
<x=1, y=3, z=-3>
*/


  lazy val answer1: Long = {
    val mutableMoon = new MutableMoon("", ImmutableMoon.initialMoons)
    println(mutableMoon.showState)
    mutableMoon.runN(1000)
    println(mutableMoon.showState)
    mutableMoon.overallEnergy
  }

  // Part 2
  lazy val answer2loop: Long = {
    val s0 = new MutableMoon("s0", ImmutableMoon.initialMoons)
    @scala.annotation.tailrec
    def loop(i: Long, s: MutableMoon): Long = {
      if(MutableMoon.eq(s0, s))
        i
      else {
        s.timeStep()
        loop(i + 1, s)
      }
    }
    val t = new MutableMoon("tortoise", ImmutableMoon.initialMoons)
    t.timeStep()
    loop(1, t)
  }

  lazy val answer2: Long = answer2floydAxisBased
  lazy val answer2floydAxisBased: Long = {
    def s0TortoiseHare = Array(
      new MutableMoon("s0", ImmutableMoon.initialMoons),
      new MutableMoon("tortoise", ImmutableMoon.initialMoons),
      new MutableMoon("hare", ImmutableMoon.initialMoons))
    val (start0, loop0) = SequenceUtils.floydMutable(s0TortoiseHare)(MutableMoon.eq(0), MutableMoon.copy)(_.timeStep())
    println(s"s:$start0, l:$loop0")
    val (start1, loop1) = SequenceUtils.floydMutable(s0TortoiseHare)(MutableMoon.eq(1), MutableMoon.copy)(_.timeStep())
    println(s"s:$start1, l:$loop1")
    val (start2, loop2) = SequenceUtils.floydMutable(s0TortoiseHare)(MutableMoon.eq(2), MutableMoon.copy)(_.timeStep())
    println(s"s:$start2, l:$loop2")
//    val div1 = PrimeNumbers.greatestCommonDivisor((start0 + loop0).toInt, (start1 + loop1).toInt)
//    println(s"d1:$div1")
//    val div2 = PrimeNumbers.greatestCommonDivisor((start0 + loop0).toInt, (start2 + loop2).toInt)
//    println(s"d2:$div2")
//    val div3 = PrimeNumbers.greatestCommonDivisor((start2 + loop2).toInt, (start1 + loop1).toInt)
//    println(s"d3:$div3")

    PrimeNumbers.leastCommonMultiple3Long((start0 + loop0),(start1 + loop1),(start2 + loop2) )
  }

  lazy val answer2floyd: Long = {
    val s0TortoiseHare = Array(
      new MutableMoon("s0", ImmutableMoon.initialMoons),
      new MutableMoon("tortoise", ImmutableMoon.initialMoons),
      new MutableMoon("hare", ImmutableMoon.initialMoons))
    val (start, loop) = SequenceUtils.floydMutable(s0TortoiseHare)(MutableMoon.eq, MutableMoon.copy)(_.timeStep())
    println(s"s:$start, l:$loop")
    start + loop
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + ImmutableMoon.answer1) // 162161676000 6678
    println("Answer1: " + answer1)
    println("Answer2: " + answer2) // 1434012655857816 // 717006327928908 // 248367250691276 // 1986938005530208 // 7947752022120832 496734501382552
  }
}
