package org.primetalk.advent

/**
  * --- Day 16: Chronal Classification ---
  *
  * As you see the Elves defend their hot chocolate successfully, you go back to falling through time. This is going to become a problem.
  *
  * If you're ever going to return to your own time, you need to understand how this device on your wrist works. You have a little while before you reach your next destination, and with a bit of trial and error, you manage to pull up a programming manual on the device's tiny screen.
  *
  * According to the manual, the device has four registers (numbered 0 through 3) that can be manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.
  *
  * Every instruction consists of four values: an opcode, two inputs (named A and B), and an output (named C), in that order. The opcode specifies the behavior of the instruction and how the inputs are interpreted. The output, C, is always treated as a register.
  *
  * In the opcode descriptions below, if something says "value A", it means to take the number given as A literally. (This is also called an "immediate" value.) If something says "register A", it means to use the number given as A to read from (or write to) the register with that number. So, if the opcode addi adds register A and value B, storing the result in register C, and the instruction addi 0 7 3 is encountered, it would add 7 to the value contained by register 0 and store the sum in register 3, never modifying registers 0, 1, or 2 in the process.
  *
  * Many opcodes are similar except for how they interpret their arguments. The opcodes fall into seven general categories:
  *
  * Addition:
  *
  * addr (add register) stores into register C the result of adding register A and register B.
  * addi (add immediate) stores into register C the result of adding register A and value B.
  *
  * Multiplication:
  *
  * mulr (multiply register) stores into register C the result of multiplying register A and register B.
  * muli (multiply immediate) stores into register C the result of multiplying register A and value B.
  *
  * Bitwise AND:
  *
  * banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
  * bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
  *
  * Bitwise OR:
  *
  * borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
  * bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
  *
  * Assignment:
  *
  * setr (set register) copies the contents of register A into register C. (Input B is ignored.)
  * seti (set immediate) stores value A into register C. (Input B is ignored.)
  *
  * Greater-than testing:
  *
  * gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
  * gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
  * gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
  *
  * Equality testing:
  *
  * eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
  * eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
  * eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
  *
  * Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number. However, you can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which. For example, suppose you capture the following sample:
  *
  * Before: [3, 2, 1, 1]
  * 9 2 1 2
  * After:  [3, 2, 2, 1]
  *
  * This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the instruction is executed, register 0 has value 3, register 1 has value 2, and registers 2 and 3 have value 1. After the instruction is executed, register 2's value becomes 2.
  *
  * The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but only three of them behave in a way that would cause the result shown in the sample:
  *
  * Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has a value of 2) produces 2, which matches the value stored in the output register, register 2.
  * Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2, which matches the value stored in the output register, register 2.
  * Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2; the number given for B is irrelevant.
  *
  * None of the other opcodes produce the result captured in the sample. Because of this, the sample above behaves like three opcodes.
  *
  * You collect many of these samples (the first section of your puzzle input). The manual also includes a small test program (the second section of your puzzle input) - you can ignore it for now.
  *
  * Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?
  *
  * Your puzzle answer was 590.
  * --- Part Two ---
  *
  * Using the samples you collected, work out the number of each opcode and execute the test program (the second section of your puzzle input).
  *
  * What value is contained in register 0 after executing the test program?
  *
  * Your puzzle answer was 475.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day16 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day16.txt")

  lazy val lines: List[String] =
    inputTextFromResource.toList

  /*

According to the manual, the device has four registers (numbered 0 through 3) that can be manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.
*/

  /** 4 registers */
  case class Registers(values: Seq[Int])

  case class OperationBinary(opcode: Int, a: Int, b: Int, outputRegister: Int)

  sealed trait InputMicroCodeType
  case object DirectInput extends InputMicroCodeType
  case object IndirectInput extends InputMicroCodeType
  case object IgnoreInput extends InputMicroCodeType
  case class OperationMicroCode(opname: String, aMicro: InputMicroCodeType, bMicro: InputMicroCodeType, f: (Int, Int) => Int)

  def cmpGreaterThan(a: Int, b: Int): Int = if(a > b) 1 else 0
  def cmpEqual(a: Int, b: Int): Int = if(a == b) 1 else 0

  val operations = Seq(
    OperationMicroCode("addr", IndirectInput, IndirectInput, _ + _),
    OperationMicroCode("addi", IndirectInput,   DirectInput, _ + _),
    OperationMicroCode("mulr", IndirectInput, IndirectInput, _ * _),
    OperationMicroCode("muli", IndirectInput,   DirectInput, _ * _),
    OperationMicroCode("banr", IndirectInput, IndirectInput, _ & _),
    OperationMicroCode("bani", IndirectInput,   DirectInput, _ & _),
    OperationMicroCode("borr", IndirectInput, IndirectInput, _ | _),
    OperationMicroCode("bori", IndirectInput,   DirectInput, _ | _),
    OperationMicroCode("setr", IndirectInput,   IgnoreInput, (a, _) => a),
    OperationMicroCode("seti", DirectInput,     IgnoreInput, (a, _) => a),
    OperationMicroCode("gtir", DirectInput,   IndirectInput, cmpGreaterThan),
    OperationMicroCode("gtri", IndirectInput,   DirectInput, cmpGreaterThan),
    OperationMicroCode("gtrr", IndirectInput, IndirectInput, cmpGreaterThan),
    OperationMicroCode("eqir", DirectInput,   IndirectInput, cmpEqual),
    OperationMicroCode("eqri", IndirectInput,   DirectInput, cmpEqual),
    OperationMicroCode("eqrr", IndirectInput, IndirectInput, cmpEqual),
  )
  /*

Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number.
However, you can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which. For example, suppose you capture the following sample:

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]
*/

  case class TestCase(before: Registers, operation: OperationBinary, after: Registers)

  val testCaseCount = 782
  def testCases: Seq[TestCase] = {
    def registers(str: String): Registers =
      Registers(parseAllIntsInString(str))
    def parseTestCase(lst: List[String]): TestCase = {
      val List(before, op, after, _) = lst
      val Seq(opCode,a,b,c) = parseAllIntsInString(op)
      TestCase(registers(before), OperationBinary(opCode, a,b,c), registers(after))
    }
    lines.take(testCaseCount * 4).sliding(4, 4).map(parseTestCase).toSeq
  }

  def evalInput(iKind: InputMicroCodeType, registers: Registers)(input: Int): Int = iKind match {
    case DirectInput => input
    case IndirectInput => registers.values(input)
    case IgnoreInput => -100
  }

  def update(c: Int, registers: Registers)(value: Int): Registers =
    Registers(registers.values.updated(c, value))

  def evalOperation(op: OperationMicroCode, binary: OperationBinary, registers: Registers): Registers = op match {
    case OperationMicroCode(_, aMicro, bMicro, f) =>
      val a = evalInput(aMicro, registers)(binary.a)
      val b = evalInput(bMicro, registers)(binary.b)
      val c = f(a, b)
      update(binary.outputRegister, registers)(c)
  }

  def checkOpAgainstTestCase(op: OperationMicroCode, testCase: TestCase): Boolean = {
    val actualAfter = evalOperation(op, testCase.operation,testCase.before)
    actualAfter == testCase.after
  }

  /*
Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?
   */
  // 590
  lazy val answer1: Long = {
    val opPerTestCase = testCases.map{tc => operations.filter(op => checkOpAgainstTestCase(op, tc))}
    opPerTestCase.count(_.size >= 3)
  }

  // Part 2
  type OpCode = Int

  def guessOpcodes(opPerTestCase: Seq[(TestCase, Seq[OperationMicroCode])]): Map[OpCode, OperationMicroCode] = {
    def go(uncertain: Map[OpCode, Seq[OperationMicroCode]], currentCodes: Map[OpCode, OperationMicroCode]): Map[OpCode, OperationMicroCode] = {
      if(uncertain.isEmpty)
        currentCodes
      else {
        val uniques = uncertain.filter(_._2.size == 1)
        if(uniques.isEmpty)
          throw new IllegalStateException("Couldn't detect opcodes for " + uncertain)
        val newOperations = uniques.values.flatten.toSet
        go(
          uncertain
            .mapValues(_.filterNot(newOperations.contains))
            .filterNot(_._2.isEmpty)
          ,
          currentCodes ++ uniques.mapValues(_.head)
        )
      }
    }
    go(opPerTestCase.map{ case (tc, op) => (tc.operation.opcode, op)}.toMap, Map())
  }

  val testProgramFirstLineNumber = 3131

  def testProgram: Seq[OperationBinary] = {
    val prog = lines.drop(testProgramFirstLineNumber - 1)
    require(prog.head == "4 0 2 0")
    prog.map{ line =>
      val Seq(op, a, b, c) = parseAllIntsInString(line)
      OperationBinary(op, a, b, c)
    }
  }

  def eval(microCodes: Map[OpCode, OperationMicroCode])(program: List[OperationBinary])(r: Registers): Registers = program match {
    case Nil => r
    case op :: tail =>
      val microCode = microCodes(op.opcode)
      val after = evalOperation(microCode, op, r)
      eval(microCodes)(tail)(after)
  }

  lazy val answer2: Long = {
    val opPerTestCase: Seq[(TestCase, Seq[OperationMicroCode])] = testCases.map{tc => (tc, operations.filter(op => checkOpAgainstTestCase(op, tc)))}
    val opcodes: Map[OpCode, OperationMicroCode] = guessOpcodes(opPerTestCase)
    val program = testProgram
    val finalRegisters = eval(opcodes)(program.toList)(Registers(Seq(0,0,0,0)))
    finalRegisters.values.head
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
