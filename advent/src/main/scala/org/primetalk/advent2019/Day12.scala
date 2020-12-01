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

  val initialState: SimulationState = SimulationState(0, initialMoons)

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
    @scala.annotation.tailrec
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
  * https://adventofcode.com/2019/day/12
  *
  * --- Day 12: The N-Body Problem ---
  *
  * The space near Jupiter is not a very safe place; you need to be careful of a big distracting red spot, extreme radiation, and a whole lot of moons swirling around. You decide to start by tracking the four largest moons: Io, Europa, Ganymede, and Callisto.
  *
  * After a brief scan, you calculate the position of each moon (your puzzle input). You just need to simulate their motion so you can avoid them.
  *
  * Each moon has a 3-dimensional position (x, y, and z) and a 3-dimensional velocity. The position of each moon is given in your scan; the x, y, and z velocity of each moon starts at 0.
  *
  * Simulate the motion of the moons in time steps. Within each time step, first update the velocity of every moon by applying gravity. Then, once all moons' velocities have been updated, update the position of every moon by applying velocity. Time progresses by one step once all of the positions are updated.
  *
  * To apply gravity, consider every pair of moons. On each axis (x, y, and z), the velocity of each moon changes by exactly +1 or -1 to pull the moons together. For example, if Ganymede has an x position of 3, and Callisto has a x position of 5, then Ganymede's x velocity changes by +1 (because 5 > 3) and Callisto's x velocity changes by -1 (because 3 < 5). However, if the positions on a given axis are the same, the velocity on that axis does not change for that pair of moons.
  *
  * Once all gravity has been applied, apply velocity: simply add the velocity of each moon to its own position. For example, if Europa has a position of x=1, y=2, z=3 and a velocity of x=-2, y=0,z=3, then its new position would be x=-1, y=2, z=6. This process does not modify the velocity of any moon.
  *
  * For example, suppose your scan reveals the following positions:
  *
  * <x=-1, y=0, z=2>
  * <x=2, y=-10, z=-7>
  * <x=4, y=-8, z=8>
  * <x=3, y=5, z=-1>
  *
  * Simulating the motion of these moons would produce the following:
  *
  * After 0 steps:
  * pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
  * pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
  * pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
  * pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>
  *
  * After 1 step:
  * pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
  * pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
  * pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
  * pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
  *
  * After 2 steps:
  * pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
  * pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
  * pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
  * pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>
  *
  * After 3 steps:
  * pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
  * pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
  * pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
  * pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>
  *
  * After 4 steps:
  * pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
  * pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
  * pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
  * pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>
  *
  * After 5 steps:
  * pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
  * pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
  * pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
  * pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>
  *
  * After 6 steps:
  * pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
  * pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
  * pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
  * pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>
  *
  * After 7 steps:
  * pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
  * pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
  * pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
  * pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>
  *
  * After 8 steps:
  * pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
  * pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
  * pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
  * pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>
  *
  * After 9 steps:
  * pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
  * pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
  * pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
  * pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>
  *
  * After 10 steps:
  * pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
  * pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
  * pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
  * pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
  *
  * Then, it might help to calculate the total energy in the system. The total energy for a single moon is its potential energy multiplied by its kinetic energy. A moon's potential energy is the sum of the absolute values of its x, y, and z position coordinates. A moon's kinetic energy is the sum of the absolute values of its velocity coordinates. Below, each line shows the calculations for a moon's potential energy (pot), kinetic energy (kin), and total energy:
  *
  * Energy after 10 steps:
  * pot: 2 + 1 + 3 =  6;   kin: 3 + 2 + 1 = 6;   total:  6 * 6 = 36
  * pot: 1 + 8 + 0 =  9;   kin: 1 + 1 + 3 = 5;   total:  9 * 5 = 45
  * pot: 3 + 6 + 1 = 10;   kin: 3 + 2 + 3 = 8;   total: 10 * 8 = 80
  * pot: 2 + 0 + 4 =  6;   kin: 1 + 1 + 1 = 3;   total:  6 * 3 = 18
  * Sum of total energy: 36 + 45 + 80 + 18 = 179
  *
  * In the above example, adding together the total energy for all moons after 10 steps produces the total energy in the system, 179.
  *
  * Here's a second example:
  *
  * <x=-8, y=-10, z=0>
  * <x=5, y=5, z=10>
  * <x=2, y=-7, z=3>
  * <x=9, y=-8, z=-3>
  *
  * Every ten steps of simulation for 100 steps produces:
  *
  * After 0 steps:
  * pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>
  *
  * After 10 steps:
  * pos=<x= -9, y=-10, z=  1>, vel=<x= -2, y= -2, z= -1>
  * pos=<x=  4, y= 10, z=  9>, vel=<x= -3, y=  7, z= -2>
  * pos=<x=  8, y=-10, z= -3>, vel=<x=  5, y= -1, z= -2>
  * pos=<x=  5, y=-10, z=  3>, vel=<x=  0, y= -4, z=  5>
  *
  * After 20 steps:
  * pos=<x=-10, y=  3, z= -4>, vel=<x= -5, y=  2, z=  0>
  * pos=<x=  5, y=-25, z=  6>, vel=<x=  1, y=  1, z= -4>
  * pos=<x= 13, y=  1, z=  1>, vel=<x=  5, y= -2, z=  2>
  * pos=<x=  0, y=  1, z=  7>, vel=<x= -1, y= -1, z=  2>
  *
  * After 30 steps:
  * pos=<x= 15, y= -6, z= -9>, vel=<x= -5, y=  4, z=  0>
  * pos=<x= -4, y=-11, z=  3>, vel=<x= -3, y=-10, z=  0>
  * pos=<x=  0, y= -1, z= 11>, vel=<x=  7, y=  4, z=  3>
  * pos=<x= -3, y= -2, z=  5>, vel=<x=  1, y=  2, z= -3>
  *
  * After 40 steps:
  * pos=<x= 14, y=-12, z= -4>, vel=<x= 11, y=  3, z=  0>
  * pos=<x= -1, y= 18, z=  8>, vel=<x= -5, y=  2, z=  3>
  * pos=<x= -5, y=-14, z=  8>, vel=<x=  1, y= -2, z=  0>
  * pos=<x=  0, y=-12, z= -2>, vel=<x= -7, y= -3, z= -3>
  *
  * After 50 steps:
  * pos=<x=-23, y=  4, z=  1>, vel=<x= -7, y= -1, z=  2>
  * pos=<x= 20, y=-31, z= 13>, vel=<x=  5, y=  3, z=  4>
  * pos=<x= -4, y=  6, z=  1>, vel=<x= -1, y=  1, z= -3>
  * pos=<x= 15, y=  1, z= -5>, vel=<x=  3, y= -3, z= -3>
  *
  * After 60 steps:
  * pos=<x= 36, y=-10, z=  6>, vel=<x=  5, y=  0, z=  3>
  * pos=<x=-18, y= 10, z=  9>, vel=<x= -3, y= -7, z=  5>
  * pos=<x=  8, y=-12, z= -3>, vel=<x= -2, y=  1, z= -7>
  * pos=<x=-18, y= -8, z= -2>, vel=<x=  0, y=  6, z= -1>
  *
  * After 70 steps:
  * pos=<x=-33, y= -6, z=  5>, vel=<x= -5, y= -4, z=  7>
  * pos=<x= 13, y= -9, z=  2>, vel=<x= -2, y= 11, z=  3>
  * pos=<x= 11, y= -8, z=  2>, vel=<x=  8, y= -6, z= -7>
  * pos=<x= 17, y=  3, z=  1>, vel=<x= -1, y= -1, z= -3>
  *
  * After 80 steps:
  * pos=<x= 30, y= -8, z=  3>, vel=<x=  3, y=  3, z=  0>
  * pos=<x= -2, y= -4, z=  0>, vel=<x=  4, y=-13, z=  2>
  * pos=<x=-18, y= -7, z= 15>, vel=<x= -8, y=  2, z= -2>
  * pos=<x= -2, y= -1, z= -8>, vel=<x=  1, y=  8, z=  0>
  *
  * After 90 steps:
  * pos=<x=-25, y= -1, z=  4>, vel=<x=  1, y= -3, z=  4>
  * pos=<x=  2, y= -9, z=  0>, vel=<x= -3, y= 13, z= -1>
  * pos=<x= 32, y= -8, z= 14>, vel=<x=  5, y= -4, z=  6>
  * pos=<x= -1, y= -2, z= -8>, vel=<x= -3, y= -6, z= -9>
  *
  * After 100 steps:
  * pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>
  * pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>
  * pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>
  * pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>
  *
  * Energy after 100 steps:
  * pot:  8 + 12 +  9 = 29;   kin: 7 +  3 + 0 = 10;   total: 29 * 10 = 290
  * pot: 13 + 16 +  3 = 32;   kin: 3 + 11 + 5 = 19;   total: 32 * 19 = 608
  * pot: 29 + 11 +  1 = 41;   kin: 3 +  7 + 4 = 14;   total: 41 * 14 = 574
  * pot: 16 + 13 + 23 = 52;   kin: 7 +  1 + 1 =  9;   total: 52 *  9 = 468
  * Sum of total energy: 290 + 608 + 574 + 468 = 1940
  *
  * What is the total energy in the system after simulating the moons given in your scan for 1000 steps?
  *
  * Your puzzle answer was 6678.
  * --- Part Two ---
  *
  * All this drifting around in space makes you wonder about the nature of the universe. Does history really repeat itself? You're curious whether the moons will ever return to a previous state.
  *
  * Determine the number of steps that must occur before all of the moons' positions and velocities exactly match a previous point in time.
  *
  * For example, the first example above takes 2772 steps before they exactly match a previous point in time; it eventually returns to the initial state:
  *
  * After 0 steps:
  * pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
  *
  * After 2770 steps:
  * pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
  * pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
  * pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
  * pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>
  *
  * After 2771 steps:
  * pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
  * pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
  * pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
  * pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>
  *
  * After 2772 steps:
  * pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
  * pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
  *
  * Of course, the universe might last for a very long time before repeating. Here's a copy of the second example from above:
  *
  * <x=-8, y=-10, z=0>
  * <x=5, y=5, z=10>
  * <x=2, y=-7, z=3>
  * <x=9, y=-8, z=-3>
  *
  * This set of initial positions takes 4686774924 steps before it repeats a previous state! Clearly, you might need to find a more efficient way to simulate the universe.
  *
  * How many steps does it take to reach the first state that exactly matches a previous state?
  *
  * Your puzzle answer was 496734501382552.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
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

    PrimeNumbers.leastCommonMultiple3Long(start0 + loop0, start1 + loop1, start2 + loop2)
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
