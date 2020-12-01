package org.primetalk.advent2019

import org.primetalk.advent.tools.Utils

trait IntCodeComputer7 {
  case class State(ip: Int, memory: Array[Int], inputs: List[Int], outputs: List[Int] = Nil) {
    def readRel(shift: Int): Int = memory(ip + shift)

    def readArg(shift: Int, isImmediate: Boolean = false): Arg =
      (
        if(isImmediate)
          ImmediateArg(_)
        else
          PositionalArg(_)
        )(readRel(shift))

    def evalArg(a: Arg): Int = a match {
      case PositionalArg(i) => memory(i)
      case ImmediateArg(i) => i
    }
  }
  // case class Arg(isImmediate: Boolean, v: Int)
  sealed trait Arg
  case class PositionalArg(i: Int) extends Arg
  case class ImmediateArg(i: Int) extends Arg

  type Interpreter = (State, Op) => State

  def aluEval(f: (Int, Int) => Int): (State, Op) => State = (s0, op) => {
    val arg1 = s0.evalArg(op.arg1)
    val arg2 = s0.evalArg(op.arg2)

    val res = f(arg1, arg2)
    s0.memory(op.resAddr) = res
    s0.copy(ip = s0.ip + opcodes(op.opcode).length)
  }

  def inputEval: (State, Op) => State = (s0, op) => {
    s0.inputs match {
      case value::nextInputs =>
        op.arg1 match {
          case PositionalArg(i) =>
            s0.memory(i) = value
            s0.copy(ip = s0.ip + opcodes(op.opcode).length, inputs = nextInputs)
          case ImmediateArg(i) =>
            throw new IllegalArgumentException(s"readEval $op has immediate argument $i at ${s0.ip}")
        }
      case _ => throw new IllegalArgumentException(s"readEval empty input @ ${s0.ip}, $op")
    }
  }
  def outputEval: (State, Op) => State = (s0, op) => {
    val value = s0.evalArg(op.arg1)
//    println(value)
    s0.copy(ip = s0.ip  + opcodes(op.opcode).length, outputs = value :: s0.outputs)
  }

  def jumpIfEval(condition: Int => Boolean): (State, Op) => State = (s0, op) => {
    val value = s0.evalArg(op.arg1)
    if(condition(value)) {
      val nextIp = s0.evalArg(op.arg2)
      s0.copy(ip = nextIp)
    } else
      s0.copy(ip = s0.ip + opcodes(op.opcode).length)
  }

  def compareEval(compare: (Int, Int) => Boolean): (State, Op) => State =
    aluEval((a,b) => if(compare(a,b)) 1 else 0)

  def haltEval: (State, Op) => State = (s0, op) => {
    s0.copy(ip = -1)
  }

  case class OpCodeInfo(code: Int, argCount: Int, eval: (State, Op) => State) {
    def length: Int = argCount + 1
  }

  val add            : OpCodeInfo = OpCodeInfo(1,  3, aluEval(_ + _))
  val mul            : OpCodeInfo = OpCodeInfo(2,  3, aluEval(_ * _))
  val input          : OpCodeInfo = OpCodeInfo(3,  1, inputEval)
  val output         : OpCodeInfo = OpCodeInfo(4,  1, outputEval)
  val `jump-if-true` : OpCodeInfo = OpCodeInfo(5,  2, jumpIfEval(_ != 0))
  val `jump-if-false`: OpCodeInfo = OpCodeInfo(6,  2, jumpIfEval(_ == 0))
  val `less than`    : OpCodeInfo = OpCodeInfo(7,  3, compareEval(_ < _)) // aluEval( if( _ < _ )1 else 0)
  val `equals`       : OpCodeInfo = OpCodeInfo(8,  3, compareEval(_ == _))
  val halt           : OpCodeInfo = OpCodeInfo(99, 0, haltEval)

  val opcodes: Map[Int, OpCodeInfo] =
    Seq(
      add, mul,
      input, output,
      `jump-if-true`, `jump-if-false`,
      `less than`, `equals`,
      halt
    ).map(o => (o.code, o)).toMap

  case class Op(opcode: Int, args: List[Arg]) { //, arg2: Arg, resAddr: PositionalArg)
    def arg1: Arg = args.head
    def arg2: Arg = args.tail.head
    def resAddr: Int = args.tail.tail.head match {
      case PositionalArg(i) => i
      case ImmediateArg(i) => throw new IllegalArgumentException(s"In $opcode resAddr is immediate($i)")
    }
  }

  def readOp(s: State): Op = {

    val i0 = s.readRel(0)
    val opcode = i0 % 100
    val info = opcodes.getOrElse(opcode, throw new IllegalArgumentException(s"Unknown opcode $opcode at ${s.ip}"))

    val positions = Map(1 -> 100, 2 -> 1000, 3 -> 10000, 4 -> 100000)
    def isImmediate(i: Int) = (i0 / positions(i)) % 10 > 0

    val args = (1 to info.argCount).map(i =>
      s.readArg(i, isImmediate(i))
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

  def runProgram(s0: State): State =
    if(s0.ip == -1)
      s0
    else {
      val s1 = run1(s0)
      runProgram(s1)
    }

  type Program = Seq[Int]

  def executeInputOutputProgram(program: Program, inputs: List[Int]): Int = {
    val s0 = State(ip = 0, program.toArray, inputs)
    val s1 = runProgram(s0)
    s1.outputs.head
  }
}
/**
  * https://adventofcode.com/2019/day/7
  *
  * --- Day 7: Amplification Circuit ---
  *
  * Based on the navigational maps, you're going to need to send more power to your ship's thrusters to reach Santa in time. To do this, you'll need to configure a series of amplifiers already installed on the ship.
  *
  * There are five amplifiers connected in series; each one receives an input signal and produces an output signal. They are connected such that the first amplifier's output leads to the second amplifier's input, the second amplifier's output leads to the third amplifier's input, and so on. The first amplifier's input value is 0, and the last amplifier's output leads to your ship's thrusters.
  *
  * O-------O  O-------O  O-------O  O-------O  O-------O
  * 0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
  * O-------O  O-------O  O-------O  O-------O  O-------O
  *
  * The Elves have sent you some Amplifier Controller Software (your puzzle input), a program that should run on your existing Intcode computer. Each amplifier will need to run a copy of the program.
  *
  * When a copy of the program starts running on an amplifier, it will first use an input instruction to ask the amplifier for its current phase setting (an integer from 0 to 4). Each phase setting is used exactly once, but the Elves can't remember which amplifier needs which phase setting.
  *
  * The program will then call another input instruction to get the amplifier's input signal, compute the correct output signal, and supply it back to the amplifier with an output instruction. (If the amplifier has not yet received an input signal, it waits until one arrives.)
  *
  * Your job is to find the largest output signal that can be sent to the thrusters by trying every possible combination of phase settings on the amplifiers. Make sure that memory is not shared or reused between copies of the program.
  *
  * For example, suppose you want to try the phase setting sequence 3,1,2,4,0, which would mean setting amplifier A to phase setting 3, amplifier B to setting 1, C to 2, D to 4, and E to 0. Then, you could determine the output signal that gets sent from amplifier E to the thrusters with the following steps:
  *
  * Start the copy of the amplifier controller software that will run on amplifier A. At its first input instruction, provide it the amplifier's phase setting, 3. At its second input instruction, provide it the input signal, 0. After some calculations, it will use an output instruction to indicate the amplifier's output signal.
  * Start the software for amplifier B. Provide it the phase setting (1) and then whatever output signal was produced from amplifier A. It will then produce a new output signal destined for amplifier C.
  * Start the software for amplifier C, provide the phase setting (2) and the value from amplifier B, then collect its output signal.
  * Run amplifier D's software, provide the phase setting (4) and input value, and collect its output signal.
  * Run amplifier E's software, provide the phase setting (0) and input value, and collect its output signal.
  *
  * The final output signal from amplifier E would be sent to the thrusters. However, this phase setting sequence may not have been the best one; another sequence might have sent a higher signal to the thrusters.
  *
  * Here are some example programs:
  *
  * Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
  *
  * 3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0
  *
  * Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
  *
  * 3,23,3,24,1002,24,10,24,1002,23,-1,23,
  * 101,5,23,23,1,24,23,23,4,23,99,0,0
  *
  * Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
  *
  * 3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
  * 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
  *
  * Try every combination of phase settings on the amplifiers. What is the highest signal that can be sent to the thrusters?
  *
  * Your puzzle answer was 212460.
  * --- Part Two ---
  *
  * It's no good - in this configuration, the amplifiers can't generate a large enough output signal to produce the thrust you'll need. The Elves quickly talk you through rewiring the amplifiers into a feedback loop:
  *
  * O-------O  O-------O  O-------O  O-------O  O-------O
  * 0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
  * |  O-------O  O-------O  O-------O  O-------O  O-------O |
  * |                                                        |
  * '--------------------------------------------------------+
  * |
  * v
  * (to thrusters)
  *
  * Most of the amplifiers are connected as they were before; amplifier A's output is connected to amplifier B's input, and so on. However, the output from amplifier E is now connected into amplifier A's input. This creates the feedback loop: the signal will be sent through the amplifiers many times.
  *
  * In feedback loop mode, the amplifiers need totally different phase settings: integers from 5 to 9, again each used exactly once. These settings will cause the Amplifier Controller Software to repeatedly take input and produce output many times before halting. Provide each amplifier its phase setting at its first input instruction; all further input/output instructions are for signals.
  *
  * Don't restart the Amplifier Controller Software on any amplifier during this process. Each one should continue receiving and sending signals until it halts.
  *
  * All signals sent or received in this process will be between pairs of amplifiers except the very first signal and the very last signal. To start the process, a 0 signal is sent to amplifier A's input exactly once.
  *
  * Eventually, the software on the amplifiers will halt after they have processed the final loop. When this happens, the last output signal from amplifier E is sent to the thrusters. Your job is to find the largest output signal that can be sent to the thrusters using the new phase settings and feedback loop arrangement.
  *
  * Here are some example programs:
  *
  * Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
  *
  * 3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
  * 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
  *
  * Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
  *
  * 3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
  * -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
  * 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
  *
  * Try every combination of the new phase settings on the amplifier feedback loop. What is the highest signal that can be sent to the thrusters?
  *
  * Your puzzle answer was 21844737.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  *
  */
object Day7 extends Utils with IntCodeComputer7 {

  val mainProgram: Program = Seq(3,8,1001,8,10,8,105,1,0,0,21,42,67,88,105,114,195,276,357,438,99999,
    3,9,101,4,9,9,102,3,9,9,1001,9,2,9,102,4,9,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,2,9,9,1002,9,
    5,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,1002,9,4,9,101,2,9,9,1002,9,2,9,4,9,99,
    3,9,101,4,9,9,102, 3,9,9,1001,9,5,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,
    3,9,101,2,9,9,4,9, 3,9,1001,9,2,9,4,9,
    3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,
    2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,
    3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,
    3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,
    3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,
    3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,
    3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,
    3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,
    3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99)

  case class Amplifier(phase: Int, program: Seq[Int]) {
    def apply(input: Int): Int =
      executeInputOutputProgram(program, List(phase, input))
  }

  def runAmplifiers(phases: Seq[Int], program: Seq[Int]): Int = {
    val amplifiers = phases.map{phase => Amplifier(phase, program)}
    amplifiers.foldLeft(0){ case (s, amp) => amp(s) }
  }

  def findLargestPermutation(len: Int, program: Seq[Int]): Int = {
    val initialList = (0 until len).toList
    initialList.permutations.map{ perm => runAmplifiers(perm, program) }.max
  }

  lazy val answer1: Long = findLargestPermutation(5, mainProgram)

  class Amplifier2(program: Program){
    private var state: State = State(ip = 0, program.toArray, inputs = Nil)

    def enqueueInput(input: Int): Unit =
      state = state.copy(inputs = state.inputs :+ input)

    // none in case of halt
    @scala.annotation.tailrec
    final def runUntilOutput(): Option[Int] = {
      state = run1(state)
      val outputs = state.outputs
      state = state.copy(outputs = Nil)
      outputs match {
        case head :: Nil =>
          Some(head)
        case Nil =>
          if(state.ip == -1)
            None
          else
            runUntilOutput()
        case _ =>
          throw new IllegalStateException(s"There are more than one output at one time: $outputs")
      }
    }
  }

  def runAmplifiers2(phases: Seq[Int], program: Seq[Int]): Int = {
    print(phases)
    val amplifiers = phases.map{ phase =>
      val amp = new Amplifier2(program)
      amp.enqueueInput(phase)
      amp
    }.toList

    @scala.annotation.tailrec
    def loop(amplifiersLeft: List[Amplifier2], signal: Int, lastSignal: Int = 0): Int = amplifiersLeft match {
      case Nil => loop(amplifiers, signal, signal)
      case amp :: t =>
        amp.enqueueInput(signal)
        amp.runUntilOutput() match {
          case None => lastSignal
          case Some(res) =>
            loop(t, res, lastSignal)
        }
    }
    val res = loop(amplifiers, 0)
    println(s": $res")
    res
  }


  def findLargestPermutation2(program: Seq[Int]): Int = {
    val initialList = (5 to 9).toList
    initialList.permutations.map{ perm => runAmplifiers2(perm, program) }.max
  }

  lazy val answer2: Long = findLargestPermutation2(mainProgram)

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
