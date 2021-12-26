package org.primetalk.advent2021

import org.primetalk.advent3.tools.Utils
import cats.parse.Parser
import org.primetalk.advent3.tools.ParsingUtils
import cats.parse.Numbers

/**
  * https://adventofcode.com/2021/day/24
  * --- Day 24: Arithmetic Logic Unit ---
  * 
  * Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU). Without the ability to perform basic arithmetic and logic functions, the submarine can't produce cool patterns with its Christmas lights!
  * 
  * It also can't navigate. Or run the oxygen system.
  * 
  * Don't worry, though - you probably have enough oxygen left to give you enough time to build a new ALU.
  * 
  * The ALU is a four-dimensional processing unit: it has integer variables w, x, y, and z. These variables all start with the value 0. The ALU also supports six instructions:
  * 
  *     inp a - Read an input value and write it to variable a.
  *     add a b - Add the value of a to the value of b, then store the result in variable a.
  *     mul a b - Multiply the value of a by the value of b, then store the result in variable a.
  *     div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
  *     mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
  *     eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
  * 
  * In all of these instructions, a and b are placeholders; a will always be the variable where the result of the operation is stored (one of w, x, y, or z), while b can be either a variable or a number. Numbers can be positive or negative, but will always be integers.
  * 
  * The ALU has no jump instructions; in an ALU program, every instruction is run exactly once in order from top to bottom. The program halts after the last instruction has finished executing.
  * 
  * (Program authors should be especially cautious; attempting to execute div with b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to crash and might even damage the ALU. These operations are never intended in any serious ALU program.)
  * 
  * For example, here is an ALU program which takes an input number, negates it, and stores it in x:
  * 
  * inp x
  * mul x -1
  * 
  * Here is an ALU program which takes two input numbers, then sets z to 1 if the second input number is three times larger than the first input number, or sets z to 0 otherwise:
  * 
  * inp z
  * inp x
  * mul z 3
  * eql z x
  * 
  * Here is an ALU program which takes a non-negative integer as input, converts it into binary, and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in w:
  * 
  * inp w
  * add z w
  * mod z 2
  * div w 2
  * add y w
  * mod y 2
  * div w 2
  * add x w
  * mod x 2
  * div w 2
  * mod w 2
  * 
  * Once you have built a replacement ALU, you can install it in the submarine, which will immediately resume what it was doing when the ALU failed: validating the submarine's model number. To do this, the ALU will run the MOdel Number Automatic Detector program (MONAD, your puzzle input).
  * 
  * Submarine model numbers are always fourteen-digit numbers consisting only of digits 1 through 9. The digit 0 cannot appear in a model number.
  * 
  * When MONAD checks a hypothetical fourteen-digit model number, it uses fourteen separate inp instructions, each expecting a single digit of the model number in order of most to least significant. (So, to check the model number 13579246899999, you would give 1 to the first inp instruction, 3 to the second inp instruction, 5 to the third inp instruction, and so on.) This means that when operating MONAD, each input instruction should only ever be given an integer value of at least 1 and at most 9.
  * 
  * Then, after MONAD has finished running all of its instructions, it will indicate that the model number was valid by leaving a 0 in variable z. However, if the model number was invalid, it will leave some other non-zero value in z.
  * 
  * MONAD imposes additional, mysterious restrictions on model numbers, and legend says the last copy of the MONAD documentation was eaten by a tanuki. You'll need to figure out what MONAD does some other way.
  * 
  * To enable as many submarine features as possible, find the largest valid fourteen-digit model number that contains no 0 digits. What is the largest model number accepted by MONAD?
  * 
  * Your puzzle answer was 92915979999498.
  * --- Part Two ---
  * 
  * As the submarine starts booting up things like the Retro Encabulator, you realize that maybe you don't need all these submarine features after all.
  * 
  * What is the smallest model number accepted by MONAD?
  * 
  * Your puzzle answer was 21611513911181.
  * 
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2124 extends Utils:

  val input = readThisObjectInputLines

  sealed trait RHV
  case class Literal(v: Int) extends RHV
  sealed trait LHV extends RHV
  case class Var(name: Char) extends LHV

  // add a b - Add the value of a to the value of b, then store the result in variable a.
  // mul a b - Multiply the value of a by the value of b, then store the result in variable a.
  // div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
  // mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
  // eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
  sealed trait BinOpType
  case object Add extends BinOpType
  case object Mul extends BinOpType
  case object Div extends BinOpType
  case object Mod extends BinOpType
  case object Eql extends BinOpType

  sealed trait Op
  case class BinOp(binOpType: BinOpType, l: Var, r: RHV) extends Op
  // inp a - Read an input value and write it to variable a.
  case class Inp(v: Var) extends Op

  def parseLhv: Parser[Var] = 
    Parser.anyChar.map(Var.apply)

  def parseLiteral: Parser[Literal] = 
    Numbers.signedIntString.map(s => Literal.apply(s.toInt))
  def parseRhv: Parser[RHV] = 
    parseLiteral | parseLhv

  def parseBinOpType: Parser[BinOpType] = 
    Parser.string("add").map(_ => Add) |
    Parser.string("mul").map(_ => Mul) |
    Parser.string("div").map(_ => Div) |
    Parser.string("mod").map(_ => Mod) |
    Parser.string("eql").map(_ => Eql)

  def parseOp: Parser[Op] =
   (
    (Parser.string("inp ") *> parseLhv).map(Inp.apply) |
      ((parseBinOpType <* Parser.char(' ')) ~ (parseLhv <* Parser.char(' ')) ~ parseRhv ) 
      .map{ case ((t,l),r) => BinOp.apply(t,l,r) }
   )
        <* Parser.char('\n')

  def parseLine(line: String): Op =
    parseOp.parseAll(line+'\n').fold(
      err => throw IllegalArgumentException(s"'$line':\n$err"),
      identity
    )
  lazy val operations = input.map(parseLine)
  lazy val digitOps = operations.sliding(18, 18).toList

  type Context = Map[Var, Long]

  val w = Var('w')
  val x = Var('x')
  val y = Var('y')
  val z = Var('z')
  val vars = List(w,x,y,z)
  val initialContext = vars.map(_ -> 0L).toMap

  def evalRhv(context: Context, rhv: RHV): Long =
    rhv match
      case Literal(i) => i
      case v@Var(_) => context(v)

  def alu(binOpType: BinOpType, v1: Long, v2: Long): Long =
    binOpType match
      case Add => v1 + v2 
      case Mul => v1 * v2 
      case Div => v1 / v2 
      case Mod => v1 % v2 
      case Eql => if v1 == v2 then 1 else 0

  def eval(context: Context, ops: List[Op], inp: List[Int]): Context =
    ops match
      case Nil => context
      case head :: tail => 
        head match
          case Inp(v) =>
            inp match
              case Nil => throw IllegalArgumentException(s"Inp is empty. context:$context, ops: $ops")
              case i::t =>
                // println(context)
                eval(context.updated(v, i), tail, t)
          case BinOp(op, v, l) =>
            val r = evalRhv(context, l)
            val old = evalRhv(context, v)
            eval(context.updated(v, alu(op, old, r)), tail, inp)
        
  val digits = List(9, 8, 7, 6, 5, 4, 3, 2, 1)

  def findDigit(ops: IndexedSeq[Op], ctx: Context): Option[(Int, Context)] =
    digits
      .map{ d => 
        val ctx2 = eval(ctx, ops.toList, d::Nil)
        println(s"D$d: $ctx2")
        (d, ctx2) }
      .find(_._2(z) == 0)
      
  def findAllDigits: (Context, List[Int]) = 
    digitOps.foldLeft((initialContext, List[Int]())){ 
      case ((ctx, digits), ops) => 
        findDigit(ops, ctx) match
          case None => throw IllegalArgumentException(s"Couldn't find digit after $digits")
          case Some((d, ctx2)) =>
            (ctx2, d::digits)
    }

  // Manual analysis of the program.
  // It consists of 14 identical functions with different parameters.
  // fun (zd, xd, yd):
  //   x = ((z % 26 + xd) == w) ==0
  //   y = (w + yd) * x
  //   z = (z / zd) * (25 * x + 1) + y
  // only `z` is used across calls.
  // The overall idea of the function is
  // - either add a new 26x digit = yd+w (NB, all yd are < 26-9 so this will fit into a single 26x digit ),
  // - or remove the previous digit if w ==%26 lastdigit(zi) + xd
  //     (otherwise mix with previous digit).
  // There are exactly 7 operations that add digit (zd == 1)
  // and exactly 7 operations that can remove or mix digit (zd == 26).
  // To get z=0 at the end, all 7 operations should be removal. 
  // This makes each of them requirements on respective digits.
  // So there will be 7 fixed relations between digits.
  // As these functions are invoked sequentially, the relations depend on one of the previous digits.
  def simplifiedFunction(xd: Int, yd: Int, zd: Int)(zi: BigInt, w: Int): BigInt =
    val x = if w == (zi % 26 + xd) then 0 else 1
    val y = (w + yd) * x
    (zi / zd) * (25 * x + 1) + y

  val simplifiedFunctions = List(
    simplifiedFunction(12, 4 , 1 ),
    simplifiedFunction(11, 11, 1 ),
    simplifiedFunction(13, 5 , 1 ),
    simplifiedFunction(11, 11, 1 ),
    simplifiedFunction(14, 14, 1 ),
    simplifiedFunction(-10,7 , 26),
    simplifiedFunction(11, 11, 1 ),
    simplifiedFunction(-9, 4 , 26),
    simplifiedFunction(-3, 6 , 26),
    simplifiedFunction(13, 5 , 1 ),
    simplifiedFunction(-5, 9 , 26),
    simplifiedFunction(-10,12, 26),
    simplifiedFunction(-4, 14, 26),
    simplifiedFunction(-5, 14, 26),
  )

  case class ExprPlus(digitIndex: Int, delta: Int)

  case class Constraint(digitIndex: Int, expr: ExprPlus):
    override def toString = 
      s"w$digitIndex == w${expr.digitIndex} + ${expr.delta}"

  def simplifiedFunction1(xd: Int, yd: Int, zd: Int)(zi: List[ExprPlus], gamma: List[Constraint], digitIndex: Int): (List[ExprPlus], List[Constraint]) =
    if zd == 1 then
      (ExprPlus(digitIndex, yd) :: zi, gamma)
    else 
      zi match
        case Nil => 
          println(s"$zi: $gamma")
          throw IllegalArgumentException()
        case ExprPlus(di, d)::t =>
          (t, Constraint(digitIndex, ExprPlus(di, d + xd))::gamma)

  val simplifiedFunction1s = List(
    simplifiedFunction1(12, 4 , 1 ),
    simplifiedFunction1(11, 11, 1 ),
    simplifiedFunction1(13, 5 , 1 ),
    simplifiedFunction1(11, 11, 1 ),
    simplifiedFunction1(14, 14, 1 ),
    simplifiedFunction1(-10,7 , 26),
    simplifiedFunction1(11, 11, 1 ),
    simplifiedFunction1(-9, 4 , 26),
    simplifiedFunction1(-3, 6 , 26),
    simplifiedFunction1(13, 5 , 1 ),
    simplifiedFunction1(-5, 9 , 26),
    simplifiedFunction1(-10,12, 26),
    simplifiedFunction1(-4, 14, 26),
    simplifiedFunction1(-5, 14, 26),
  )
 
  def findMaxDigitsForConstraints(list: List[Constraint]): IndexedSeq[(Int, Int)] =
    for
      i <- 0 to 13
      constraints = list.filter(c => c.digitIndex == i || c.expr.digitIndex == i)
    yield
      i -> constraints.headOption.
        map{ 
          case Constraint(di, ExprPlus(di2, delta))  =>
            def max = math.min(9 - delta, 9)
            if i < di then  // towards max.
              max
            else
              max + delta
        }.
        getOrElse(9)// when there are no constraints - select 9

  def findMinDigitsForConstraints(list: List[Constraint]): IndexedSeq[(Int, Int)] =
    for
      i <- 0 to 13
      constraints = list.filter(c => c.digitIndex == i || c.expr.digitIndex == i)
    yield
      i -> constraints.headOption.
        map{ 
          case Constraint(di, ExprPlus(di2, delta))  =>
            def min = math.max(1 - delta, 1)
            if i < di then  // towards min.
              min
            else
              min + delta
        }.
        getOrElse(1)// when there are no constraints - select 9

  val (_, gamma) = simplifiedFunction1s.zipWithIndex.foldLeft((List[ExprPlus](), List[Constraint]())){
    case ((z, gamma), (f, i)) =>
      f(z, gamma, i)
  }
  lazy val answer1: String = 
    // println(gamma.sortBy(_.digitIndex).mkString("\n"))
    findMaxDigitsForConstraints(gamma).sortBy(_._1).map(_._2).mkString

  //Part 2
  lazy val answer2: String = 
    findMinDigitsForConstraints(gamma).sortBy(_._1).map(_._2).mkString

    // val c9 = eval(initialContext, operations.toList, digits)//List(9,9,9,9,9,4,3,2,1,9,8,7,6,5))
    // println(c9)
    // ""

  def main(args: Array[String]): Unit =
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
