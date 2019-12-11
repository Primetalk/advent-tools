package org.primetalk.advent2019

import org.primetalk.advent.tools.Utils

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait IntCodeComputer9 {

  type Word = Long

  sealed trait Memory[W] {
    def apply(addr: W): W
    def update(addr: W, v: W): Unit
  }
  class SimpleMemory[W: Numeric: ClassTag](initial: Seq[W], maxSize: Int = 1000000) extends Memory[W] {

    private val arr: Array[W] = {
      val res = Array.fill[W](maxSize)(implicitly[Numeric[W]].zero)
      Array.copy(initial.toArray, 0, res, 0, initial.size)
      res
    }

    override def apply(addr: W): W = arr(implicitly[Numeric[W]].toInt(addr))

    override def update(addr: W, v: W): Unit = {
      arr(implicitly[Numeric[W]].toInt(addr)) = v
    }
  }
//  class ExpandableMemory[W](arr: Array[W]) extends Memory[W] {
//    override def apply(addr: W)(implicit n: Numeric[W]): W = arr(n.toInt(addr))
//
//    override def update(addr: W, v: W)(implicit n: Numeric[W]): Unit = {
//      arr(n.toInt(addr)) = v
//    }
//  }
  /**
    *
    * @param ip instruction pointer
    * @param rb relative base
    * @param memory the advanced memory
    * @param inputs list of inputs
    * @param outputs produced outputs
    */
  case class State(ip: Word, rb: Word, memory: Memory[Word], inputs: List[Word], outputs: List[Word] = Nil) {
    def readRel(shift: Word): Word = memory(ip + shift)

    def readArg(shift: Word, mode: Int): Arg = {
      val argValue = readRel(shift)
      mode match {
        case 0 => PositionalArg(argValue)
        case 1 => ImmediateArg(argValue)
        case 2 => RelativeArg(argValue)
      }
    }

    def evalArg(a: Arg): Word = a match {
      case PositionalArg(i) => memory(i)
      case ImmediateArg(i) => i
      case RelativeArg(i) => memory(rb + i)
    }
    def enqueue(i: Word): State =
      copy(inputs = inputs :+ i)
    def dequeueOutput: (State, Word) =
      outputs match {
        case h::t => (copy(outputs = t), h)
        case Nil => throw new IllegalStateException("No output")
      }
  }

  sealed trait Arg
  case class PositionalArg(i: Word) extends Arg
  case class ImmediateArg(i: Word) extends Arg
  case class RelativeArg(i: Word) extends Arg

  type Interpreter = (State, Op) => State

  def aluEval(f: (Word, Word) => Word): (State, Op) => State = (s0, op) => {
    val arg1 = s0.evalArg(op.arg1)
    val arg2 = s0.evalArg(op.arg2)

    val res = f(arg1, arg2)

    s0.memory(op.resAddr(s0.rb)) = res
    s0.copy(ip = s0.ip + opcodes(op.opcode).length)
  }

  def inputEval: (State, Op) => State = (s0, op) => {
    s0.inputs match {
      case value::nextInputs =>
        op.arg1 match {
          case RelativeArg(i) =>
            s0.memory(s0.rb + i) = value
          case PositionalArg(i) =>
            s0.memory(i) = value
          case ImmediateArg(i) =>
            throw new IllegalArgumentException(s"readEval $op has immediate argument $i at ${s0.ip}")
        }
        s0.copy(ip = s0.ip + opcodes(op.opcode).length, inputs = nextInputs)
      case _ => throw new IllegalArgumentException(s"readEval empty input @ ${s0.ip}, $op")
    }
  }

  def outputEval: (State, Op) => State = (s0, op) => {
    val value = s0.evalArg(op.arg1)
//    println(value)
    s0.copy(ip = s0.ip  + opcodes(op.opcode).length, outputs = value :: s0.outputs)
  }

  def jumpIfEval(condition: Word => Boolean): (State, Op) => State = (s0, op) => {
    val value = s0.evalArg(op.arg1)
    if(condition(value)) {
      val nextIp = s0.evalArg(op.arg2)
      s0.copy(ip = nextIp)
    } else
      s0.copy(ip = s0.ip + opcodes(op.opcode).length)
  }

  def compareEval(compare: (Word, Word) => Boolean): (State, Op) => State =
    aluEval((a,b) => if(compare(a,b)) 1 else 0)

  def relativeBaseAddToEval: (State, Op) => State = {
    case (s0, Op(opcode, List(arg1))) =>
      s0.copy(ip = s0.ip + opcodes(opcode).length, rb = s0.rb + s0.evalArg(arg1))
  }

  def haltEval: (State, Op) => State = (s0, op) => {
    s0.copy(ip = -1)
  }

  case class OpCodeInfo(code: Int, argCount: Int, eval: (State, Op) => State) {
    def length: Int = argCount + 1
  }

  val add             = OpCodeInfo(1,  3, aluEval(_ + _))
  val mul             = OpCodeInfo(2,  3, aluEval(_ * _))
  val input           = OpCodeInfo(3,  1, inputEval)
  val output          = OpCodeInfo(4,  1, outputEval)
  val `jump-if-true`  = OpCodeInfo(5,  2, jumpIfEval(_ != 0))
  val `jump-if-false` = OpCodeInfo(6,  2, jumpIfEval(_ == 0))
  val `less than`     = OpCodeInfo(7,  3, compareEval(_ < _)) // aluEval( if( _ < _ )1 else 0)
  val `equals`        = OpCodeInfo(8,  3, compareEval(_ == _))
  val addToRelativeBase=OpCodeInfo(9,  1, relativeBaseAddToEval)
  val halt            = OpCodeInfo(99, 0, haltEval)

  val opcodes: Map[Int, OpCodeInfo] =
    Seq(
      add, mul,
      input, output,
      `jump-if-true`, `jump-if-false`,
      `less than`, `equals`,
      addToRelativeBase,
      halt
    ).map(o => (o.code, o)).toMap

  case class Op(opcode: Int, args: List[Arg]) { //, arg2: Arg, resAddr: PositionalArg)
    def arg1: Arg = args.head
    def arg2: Arg = args.tail.head
    def resAddr(rb: Word): Word = args.tail.tail.head match {
      case PositionalArg(i) => i
      case ImmediateArg(i) => throw new IllegalArgumentException(s"In $opcode resAddr is ImmediateArg($i)")
      case RelativeArg(i) => i + rb
    }
  }

  def readOp(s: State): Op = {

    val i0 = s.readRel(0)
    val opcode = (i0 % 100).toInt
    val info = opcodes.getOrElse(opcode, throw new IllegalArgumentException(s"Unknown opcode $opcode at ${s.ip}"))

    val positions = Map(1 -> 100, 2 -> 1000, 3 -> 10000, 4 -> 100000)
    def mode(i: Int): Int = ((i0 / positions(i)) % 10).toInt

    val args = (1 to info.argCount).map(i =>
      s.readArg(i, mode(i))
    ).toList
    Op(opcode, args)
  }

  def eval(s0: State, op: Op): State = {
    val info = opcodes.getOrElse(op.opcode, throw new IllegalArgumentException(s"Unknown opcode $op"))
    info.eval(s0, op)
  }

  def run1(s0: State): State = {
    if(s0.ip == -1) s0
    else {
      val op = readOp(s0)
      eval(s0, op)
    }
  }

  @tailrec
  final def runProgram(s0: State): State =
    if(s0.ip == -1)
      s0
    else {
      val s1 = run1(s0)
      runProgram(s1)
    }

  type Program = Seq[Word]

  def runUntilOutputOrFinish(s0: State): State = {
    if(s0.ip == -1 || s0.outputs.nonEmpty)
      s0
    else {
      val s1 = run1(s0)
      runUntilOutputOrFinish(s1)
    }
  }
  def executeInputOutputProgram(program: Program, inputs: List[Word]): Word = {
    val s0 = State(ip = 0, rb = 0, new SimpleMemory(program), inputs)
    val s1 = runProgram(s0)
    s1.outputs.head
  }
}
/**
  * https://adventofcode.com/2019/day/9
  *
  * --- Day 9: Sensor Boost ---
  *
  * You've just said goodbye to the rebooted rover and left Mars when you receive a faint distress signal coming from the asteroid belt. It must be the Ceres monitoring station!
  *
  * In order to lock on to the signal, you'll need to boost your sensors. The Elves send up the latest BOOST program - Basic Operation Of System Test.
  *
  * While BOOST (your puzzle input) is capable of boosting your sensors, for tenuous safety reasons, it refuses to do so until the computer it runs on passes some checks to demonstrate it is a complete Intcode computer.
  *
  * Your existing Intcode computer is missing one key feature: it needs support for parameters in relative mode.
  *
  * Parameters in mode 2, relative mode, behave very similarly to parameters in position mode: the parameter is interpreted as a position. Like position mode, parameters in relative mode can be read from or written to.
  *
  * The important difference is that relative mode parameters don't count from address 0. Instead, they count from a value called the relative base. The relative base starts at 0.
  *
  * The address a relative mode parameter refers to is itself plus the current relative base. When the relative base is 0, relative mode parameters and position mode parameters with the same value refer to the same address.
  *
  * For example, given a relative base of 50, a relative mode parameter of -7 refers to memory address 50 + -7 = 43.
  *
  * The relative base is modified with the relative base offset instruction:
  *
  * Opcode 9 adjusts the relative base by the value of its only parameter. The relative base increases (or decreases, if the value is negative) by the value of the parameter.
  *
  * For example, if the relative base is 2000, then after the instruction 109,19, the relative base would be 2019. If the next instruction were 204,-34, then the value at address 1985 would be output.
  *
  * Your Intcode computer will also need a few other capabilities:
  *
  * The computer's available memory should be much larger than the initial program. Memory beyond the initial program starts with the value 0 and can be read or written like any other memory. (It is invalid to try to access memory at a negative address, though.)
  * The computer should have support for large numbers. Some instructions near the beginning of the BOOST program will verify this capability.
  *
  * Here are some example programs that use these features:
  *
  * 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input and produces a copy of itself as output.
  * 1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
  * 104,1125899906842624,99 should output the large number in the middle.
  *
  * The BOOST program will ask for a single input; run it in test mode by providing it the value 1. It will perform a series of checks on each opcode, output any opcodes (and the associated parameter modes) that seem to be functioning incorrectly, and finally output a BOOST keycode.
  *
  * Once your Intcode computer is fully functional, the BOOST program should report no malfunctioning opcodes when run in test mode; it should only output a single value, the BOOST keycode. What BOOST keycode does it produce?
  *
  * Your puzzle answer was 2671328082.
  * --- Part Two ---
  *
  * You now have a complete Intcode computer.
  *
  * Finally, you can lock on to the Ceres distress signal! You just need to boost your sensors using the BOOST program.
  *
  * The program runs in sensor boost mode by providing the input instruction the value 2. Once run, it will boost the sensors automatically, but it might take a few seconds to complete the operation on slower hardware. In sensor boost mode, the program will output a single value: the coordinates of the distress signal.
  *
  * Run the BOOST program in sensor boost mode. What are the coordinates of the distress signal?
  *
  * Your puzzle answer was 59095.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  */
object Day9 extends Utils with IntCodeComputer9 {

  val i = 1125899906842624L
  val printItself: Program = Seq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  val mainProgram: Program = Seq(1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,24,1017,1101,0,36,1006,1101,0,30,1011,1101,26,0,1018,1101,32,0,1015,1101,34,0,1004,1101,0,37,1002,1101,25,0,1012,1102,38,1,1010,1101,29,0,1019,1101,308,0,1029,1102,1,696,1027,1102,1,429,1022,
    1102,1,21,1005,1102,1,33,1013,1101,39,0,1008,1102,20,1,1009,1101,0,652,1025,1102,313,1,1028,1101,0,31,1003,1102,661,1,1024,1101,35,0,1016,1101,0,23,1000,1102,28,1,1014,1102,0,1,1020,1102,27,1,1007,1101,0,1,1021,1102,22,1,1001,1101,703,0,1026,1101,0,422,1023,109,-5,2101,0,9,63,1008,63,31,63,1005,63,205,1001,64,1,64,1105,1,207,4,187,1002,64,2,64,109,6,2102,1,3,63,1008,63,37,63,1005,63,227,1105,1,233,4,213,1001,64,1,64,1002,64,2,64,109,11,21108,40,40,3,1005,1015,255,4,239,
    1001,64,1,64,1106,0,255,1002,64,2,64,109,-3,21107,41,40,2,1005,1011,275,1001,64,1,64,1105,1,277,4,261,1002,64,2,64,109,4,2107,28,-6,63,1005,63,297,1001,64,1,64,
    1106,0,299,4,283,1002,64,2,64,109,15,2106,0,0,4,305,1106,0,317,1001,64,1,64,1002,64,2,64,109,-23,2108,22,4,63,1005,63,337,1001,64,1,64,1105,1,339,4,323,1002,64,2,64,109,6,21101,42,0,0,1008,1011,40,63,1005,63,363,1001,64,1,64,1105,1,365,4,345,1002,64,2,64,109,-17,1207,7,21,63,1005,63,381,1105,1,387,4,371,1001,64,1,64,1002,64,2,64,109,14,1201,-1,0,63,1008,63,25,63,1005,63,407,1105,1,413,4,393,1001,64,1,64,1002,64,2,64,109,15,2105,1,0,1001,64,1,64,1105,1,431,4,419,
    1002,64,2,64,109,-23,2101,0,6,63,1008,63,36,63,1005,63,453,4,437,1106,0,457,1001,64,1,64,1002,64,2,64,109,10,2108,21,-5,63,1005,63,475,4,463,1106,0,479,1001,64,1,64,1002,64,2,64,109,-3,1201,2,0,63,1008,63,20,63,1005,63,505,4,485,1001,64,1,64,1105,1,505,1002,64,2,64,109,4,2107,35,-5,63,1005,63,527,4,511,1001,64,1,64,1105,1,527,1002,64,2,64,109,15,1206,-5,543,1001,64,1,64,1105,1,545,4,533,1002,64,2,64,109,-8,1205,3,563,4,551,1001,64,1,64,1106,0,563,1002,64,2,64,109,-5,
    1206,7,581,4,569,1001,64,1,64,1105,1,581,1002,64,2,64,109,-8,1207,-3,38,63,1005,63,599,4,587,1105,1,603,1001,64,1,64,1002,64,2,64,109,19,1205,-4,619,1001,64,1,64,1105,1,621,4,609,1002,64,2,64,109,-13,1208,-4,27,63,1005,63,639,4,627,1105,1,643,1001,64,1,64,1002,64,2,64,109,5,2105,1,8,4,649,1001,64,1,64,1106,0,661,1002,64,2,64,109,-16,1202,4,1,63,1008,63,34,63,1005,63,683,4,667,1106,0,687,1001,64,1,64,1002,64,2,64,109,26,2106,0,1,1001,64,1,64,1105,1,705,4,693,
    1002,64,2,64,109,-9,21102,43,1,-7,1008,1010,46,63,1005,63,725,1105,1,731,4,711,1001,64,1,64,1002,64,2,64,109,-26,1202,9,1,63,1008,63,26,63,1005,63,755,1001,64,1,64,1105,1,757,4,737,1002,64,2,64,109,34,21108,44,43,-8,1005,1017,773,1106,0,779,4,763,1001,64,1,64,1002,64,2,64,109,-15,21102,45,1,1,1008,1011,45,63,1005,63,801,4,785,1106,0,805,1001,64,1,64,1002,64,2,64,109,-14,1208,10,35,63,1005,63,821,1106,0,827,4,811,1001,64,1,64,1002,64,2,64,109,17,2102,1,-4,63,
    1008,63,20,63,1005,63,853,4,833,1001,64,1,64,1106,0,853,1002,64,2,64,109,6,21107,46,47,-4,1005,1015,871,4,859,1105,1,875,1001,64,1,64,1002,64,2,64,109,-10,
    21101,47,0,4,1008,1013,47,63,1005,63,901,4,881,1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21102,1,915,0,1106,0,922,21201,1,37790,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,22102,1,1,-1,21201,-2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0)

  lazy val answer1: Word = {
    val in = 1
    val s0 = State(ip = 0, rb = 0, memory = new SimpleMemory(mainProgram), inputs = in :: Nil)
    val s1 = runProgram(s0)
    s1.outputs.head
  }

  lazy val answer2: Long =  {
    val in = 2
    val s0 = State(ip = 0, rb = 0, memory = new SimpleMemory(mainProgram), inputs = in :: Nil)
    val s1 = runProgram(s0)
    s1.outputs.head
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
