package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils.{unfoldUntil, unfoldWhile}
import org.primetalk.advent.tools.{PrimeNumbers, Utils}

import scala.annotation.tailrec
import scala.util.Try

/**
  * --- Day 19: Go With The Flow ---
  *
  * With the Elves well on their way constructing the North Pole base, you turn your attention back to understanding the inner workings of programming the device.
  *
  * You can't help but notice that the device's opcodes don't contain any flow control like jump instructions. The device's manual goes on to explain:
  *
  * "In programs where flow control is required, the instruction pointer can be bound to a register so that it can be manipulated directly. This way, setr/seti can function as absolute jumps, addr/addi can function as relative jumps, and other opcodes can cause truly fascinating effects."
  *
  * This mechanism is achieved through a declaration like #ip 1, which would modify register 1 so that accesses to it let the program indirectly access the instruction pointer itself. To compensate for this kind of binding, there are now six registers (numbered 0 through 5); the five not bound to the instruction pointer behave as normal. Otherwise, the same rules apply as the last time you worked with this device.
  *
  * When the instruction pointer is bound to a register, its value is written to that register just before each instruction is executed, and the value of that register is written back to the instruction pointer immediately after each instruction finishes execution. Afterward, move to the next instruction by adding one to the instruction pointer, even if the value in the instruction pointer was just updated by an instruction. (Because of this, instructions must effectively set the instruction pointer to the instruction before the one they want executed next.)
  *
  * The instruction pointer is 0 during the first instruction, 1 during the second, and so on. If the instruction pointer ever causes the device to attempt to load an instruction outside the instructions defined in the program, the program instead immediately halts. The instruction pointer starts at 0.
  *
  * It turns out that this new information is already proving useful: the CPU in the device is not very powerful, and a background process is occupying most of its time. You dump the background process' declarations and instructions to a file (your puzzle input), making sure to use the names of the opcodes rather than the numbers.
  *
  * For example, suppose you have the following program:
  *
  * #ip 0
  * seti 5 0 1
  * seti 6 0 2
  * addi 0 1 0
  * addr 1 2 3
  * setr 1 0 0
  * seti 8 0 4
  * seti 9 0 5
  *
  * When executed, the following instructions are executed. Each line contains the value of the instruction pointer at the time the instruction started, the values of the six registers before executing the instructions (in square brackets), the instruction itself, and the values of the six registers after executing the instruction (also in square brackets).
  *
  * ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
  * ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
  * ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
  * ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
  * ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]
  *
  * In detail, when running this program, the following events occur:
  *
  * The first line (#ip 0) indicates that the instruction pointer should be bound to register 0 in this program. This is not an instruction, and so the value of the instruction pointer does not change during the processing of this line.
  * The instruction pointer contains 0, and so the first instruction is executed (seti 5 0 1). It updates register 0 to the current instruction pointer value (0), sets register 1 to 5, sets the instruction pointer to the value of register 0 (which has no effect, as the instruction did not modify register 0), and then adds one to the instruction pointer.
  * The instruction pointer contains 1, and so the second instruction, seti 6 0 2, is executed. This is very similar to the instruction before it: 6 is stored in register 2, and the instruction pointer is left with the value 2.
  * The instruction pointer is 2, which points at the instruction addi 0 1 0. This is like a relative jump: the value of the instruction pointer, 2, is loaded into register 0. Then, addi finds the result of adding the value in register 0 and the value 1, storing the result, 3, back in register 0. Register 0 is then copied back to the instruction pointer, which will cause it to end up 1 larger than it would have otherwise and skip the next instruction (addr 1 2 3) entirely. Finally, 1 is added to the instruction pointer.
  * The instruction pointer is 4, so the instruction setr 1 0 0 is run. This is like an absolute jump: it copies the value contained in register 1, 5, into register 0, which causes it to end up in the instruction pointer. The instruction pointer is then incremented, leaving it at 6.
  * The instruction pointer is 6, so the instruction seti 9 0 5 stores 9 into register 5. The instruction pointer is incremented, causing it to point outside the program, and so the program ends.
  *
  * What value is left in register 0 when the background process halts?
  *
  * Your puzzle answer was 1530.
  * --- Part Two ---
  *
  * A new background process immediately spins up in its place. It appears identical, but on closer inspection, you notice that this time, register 0 started with the value 1.
  *
  * What value is left in register 0 when this new background process halts?
  *
  * Your puzzle answer was 16533000.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
trait Day19Program extends Utils with Day19DeviceEmulator {

  def lines: Seq[String]

  lazy val programSpecification: ProgramSpecification = parseProgramSpecification(lines)

  lazy val initialIpSelector: IpSelector = programSpecification.ipSelector

  lazy val inputProgram: Vector[OperationBinary] = programSpecification.instructions.toVector

}
trait Day19DeviceEmulator {
  type Word = Long
  type IP = Word
  type RegisterId = Int

  type IpSelector = RegisterId
  type OpName = String

  case class ProgramSpecification(ipSelector: IpSelector, instructions: Seq[OperationBinary])
  /* there are now six registers (numbered 0 through 5) */
  val registerCount = 6

  /** 6 registers */
  case class Registers(values: Seq[Word] = 0L until registerCount, ipRegisterSelector: Int, ip: Word, executedInstructions: Long = 0L) {
    def copyIpToRegister: Registers = this.copy(values = values.updated(ipRegisterSelector, ip))

    def copyFromRegisterToIpAndMoveNext: Registers = this.copy(ip = values(ipRegisterSelector) + 1)
  }

  case class OperationBinary(opname: OpName, a: Int, b: Int, outputRegister: Int)

  sealed trait ArgMicroCodeType

  case object ImmediateArg extends ArgMicroCodeType

  case object RegisterArg extends ArgMicroCodeType

  case object IgnoreArg extends ArgMicroCodeType

  case class OperationMicroCode(opname: OpName, aMicro: ArgMicroCodeType, bMicro: ArgMicroCodeType, f: (Word, Word) => Word)

  def cmpGreaterThan(a: Word, b: Word): Word = if (a > b) 1L else 0L

  def cmpEqual(a: Word, b: Word): Word = if (a == b) 1L else 0L

  val operations = Seq(
    OperationMicroCode("addr", RegisterArg, RegisterArg, _ + _),
    OperationMicroCode("addi", RegisterArg, ImmediateArg, _ + _),
    OperationMicroCode("mulr", RegisterArg, RegisterArg, _ * _),
    OperationMicroCode("muli", RegisterArg, ImmediateArg, _ * _),
    OperationMicroCode("banr", RegisterArg, RegisterArg, _ & _),
    OperationMicroCode("bani", RegisterArg, ImmediateArg, _ & _),
    OperationMicroCode("borr", RegisterArg, RegisterArg, _ | _),
    OperationMicroCode("bori", RegisterArg, ImmediateArg, _ | _),
    OperationMicroCode("setr", RegisterArg, IgnoreArg, (a, _) => a),
    OperationMicroCode("seti", ImmediateArg, IgnoreArg, (a, _) => a),
    OperationMicroCode("gtir", ImmediateArg, RegisterArg, cmpGreaterThan),
    OperationMicroCode("gtri", RegisterArg, ImmediateArg, cmpGreaterThan),
    OperationMicroCode("gtrr", RegisterArg, RegisterArg, cmpGreaterThan),
    OperationMicroCode("eqir", ImmediateArg, RegisterArg, cmpEqual),
    OperationMicroCode("eqri", RegisterArg, ImmediateArg, cmpEqual),
    OperationMicroCode("eqrr", RegisterArg, RegisterArg, cmpEqual),
  )

  val microCodes: Map[OpName, OperationMicroCode] =
    operations.map(o => (o.opname, o)).toMap
  def evalInput(iKind: ArgMicroCodeType, registers: Registers)(input: Int): Word = iKind match {
    case ImmediateArg => input
    case RegisterArg => registers.values(input)
    case IgnoreArg => -100L
  }

  def update(c: Int, registers: Registers)(value: Word): Registers =
    registers.copy(
      values = registers.values.updated(c, value),
      executedInstructions = registers.executedInstructions + 1L
    )

  def evalOperation(op: OperationMicroCode, binary: OperationBinary, registers: Registers): Registers = op match {
    case OperationMicroCode(_, aMicro, bMicro, f) =>
      val a = evalInput(aMicro, registers)(binary.a)
      val b = evalInput(bMicro, registers)(binary.b)
      val c = f(a, b)
      update(binary.outputRegister, registers)(c)
  }

  def executeOneInstruction(program: Vector[OperationBinary])(r: Registers): Registers = {
    val ip = r.ip
    if (ip >= 0 && ip < program.length) {
      val op = program(ip.toInt)
      val microCode = microCodes(op.opname)
      val registers = r.copyIpToRegister
      val after = evalOperation(microCode, op, registers)
      val moveIp = after.copyFromRegisterToIpAndMoveNext
      moveIp
    } else
      throw new IllegalStateException("Cannot proceed with " + r)
  }

  final def eval(program: Vector[OperationBinary])(r: Registers): Registers = {
    unfoldWhile(r)(executeOneInstruction(program), rr => rr.ip >= 0 && rr.ip < program.length)
  }

  def parseProgramSpecification(lines: Seq[String]): ProgramSpecification = {
    val initialIpSelector: IpSelector =
      Utils.parseAllIntsInString(lines.head).head

    val inputProgram: Vector[OperationBinary] = lines.tail.map { line =>
      val opname = line.take(4)
      val Seq(a, b, c) = Utils.parseAllIntsInString(line)
      OperationBinary(opname, a, b, c)
    }.toVector
    ProgramSpecification(initialIpSelector, inputProgram)
  }
}
abstract class Day19 extends Day19Program {
  /*
What value is left in register 0 when the background process halts?
   */
  lazy val answer1: Long = {
    val finalRegisters = eval(inputProgram)(Registers(Seq(0L,0,0,0,0,0), initialIpSelector, 0L))
    finalRegisters.values.head
  }

  def instr(r: Registers): OperationBinary = inputProgram(r.ip.toInt)

  /** For debug purposes.*/
  def runInfinitely(): Nothing = {
    val initialRegisters = Registers(Seq(1L, 0, 0, 0, 0, 0), initialIpSelector, 0L)
    val step = executeOneInstruction(inputProgram) _
    @tailrec
    def go(r: Registers): Nothing = {
      var nextR = step(r) // var is intentional, so that we can modify it during debug session.
      println(r.toString + "\t" + instr(r).toString + "\t" + Try(nextR).toString)
      go(nextR)
    }
    go(initialRegisters)
  }
  // Part 2
  // 10551419 - is too low
  // 1 + 10551418 + 10551418 =  21102837 - is too high
  // 16533001
  // 16533000 - sum of divisors
  lazy val answer2: Long = {
    val initialRegisters = Registers(Seq(1L, 0, 0, 0, 0, 0), initialIpSelector, 0L)
    val step = executeOneInstruction(inputProgram) _
    val rBeforeLoop: Registers = unfoldUntil(step)(_.ip == 1)(initialRegisters)
    val r2: Word = rBeforeLoop.values(2)
    require(r2 == 10551418) // the number that we expect in our case
    val divisors = PrimeNumbers.allDivisors(r2.toInt)
    // Seq(1, 2, 29, 58, 109, 218, 1669, 3161, 3338, 6322, 48401, 96802, 181921, 363842, 5275709, 10551418)
    divisors.sum
  }
}
object Day19 extends Day19 {
  lazy val inputTextFromResource: Iterator[String] =
    readResource("day19.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

  /*
  10551418 = 2 *  29 * 109 * 1669 There are 16 divisors
Program structure - to a high level
00: addi 3 16 3 jump 17                   init() // POST: r0 = 0 r2 = 10551418; r5 = 10550400 - ignored.
01: seti 1 3 1  r1 = 1                    for(r1 = 1; r1 <= r2; r1 ++) {
02: seti 1 2 4  r4 = 1                    for(r4 = 1; r4 <= r2; r4 ++) {
03: mulr 1 4 5  r5 = r1 * r4                if(r1 * r4 == r2) r0 = r0 + r1 // this happens on primary factors as well.
04: eqrr 5 2 5  r5 = r5 == r2
05: addr 5 3 3  if(r5 == r2) jump 07
06: addi 3 1 3  jump 08
07: addr 1 0 0  r0 = r0 + r1
08: addi 4 1 4  inc r4
09: gtrr 4 2 5  r5 = r4 > r2
10: addr 3 5 3  if(r4 > r2) jump 12
11: seti 2 6 3  jump 03                   loop r4
12: addi 1 1 1  inc r1
13: gtrr 1 2 5  r5 = r1 > r2
14: addr 5 3 3  if(r1 > r2) jump 16
15: seti 1 0 3  jump 02                   }}
16: mulr 3 3 3  halt                      halt // after the loop we stop execution
// def init()
17: addi 2 2 2  r2 = r2 + 2
18: mulr 2 2 2
19: mulr 3 2 2
20: muli 2 11 2
21: addi 5 8 5
22: mulr 5 3 5
23: addi 5 6 5
24: addr 2 5 2
25: addr 3 0 3
26: seti 0 5 3
27: setr 3 0 5
28: mulr 5 3 5
29: addr 3 5 5
30: mulr 3 5 5
31: muli 5 14 5
32: mulr 5 3 5
33: addr 2 5 2
34: seti 0 8 0 r0 = 0   // this is actually part of main program
35: seti 0 9 3 jump 01
    */
}
