package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils
import ru.primetalk.rewritabletree.RewritableTree

import scala.collection.immutable

object Decompiler extends Day19DeviceEmulator {

  sealed trait Expression0
  sealed trait Expression[T] extends Expression0

  sealed trait BinaryOperation[T] extends Expression[(Word, Word) => T]

  case object Add extends BinaryOperation[Word]
  case object Mul extends BinaryOperation[Word]
  case object Div extends BinaryOperation[Word]

  case object BinAnd extends BinaryOperation[Word]
  case object BinOr extends BinaryOperation[Word]

  sealed trait LogicBinaryOperation extends BinaryOperation[Boolean]
  case object Greater extends LogicBinaryOperation
  case object EqualsTo extends LogicBinaryOperation

  case class ReadRegister(reg: RegisterId) extends Expression[Word]
  case class ReadBoolean(reg: RegisterId) extends Expression[Boolean]
  case class Const[T](value: T) extends Expression[T]

  case class Apply2[A,B,C](op: Expression[(A, B) => C])(val a: Expression[A], val b: Expression[B]) extends Expression[C]

  sealed trait BinaryStatement
  case class SetWord(registerId: RegisterId, expression: Expression[Word]) extends BinaryStatement
  case class SetBoolean(registerId: RegisterId, expression: Expression[Boolean]) extends BinaryStatement
  case class JumpRelative(expression: Expression[Word]) extends BinaryStatement
  case class Jump(pos: IP) extends BinaryStatement

  case class LabelledBinaryStatement(pos: IP, binaryStatement: BinaryStatement)

  def convert(ipSelector: IpSelector)(operationBinary: OperationBinary): BinaryStatement = operationBinary match {
    case OperationBinary(opname, a, b, outputRegister) =>
//      val microCode: OperationMicroCode = microCodes(opname)
      if(outputRegister == ipSelector)
        opname match {
          case "addr" => JumpRelative(Apply2(Add)(ReadRegister(a), ReadRegister(b)))
          case "addi" => JumpRelative(Apply2(Add)(ReadRegister(a), Const(b)))
          case "mulr" => JumpRelative(Apply2(Mul)(ReadRegister(a), ReadRegister(b)))
          case "muli" => JumpRelative(Apply2(Mul)(ReadRegister(a), Const(b)))
          case "banr" => JumpRelative(Apply2(BinAnd)(ReadRegister(a), ReadRegister(b)))
          case "bani" => JumpRelative(Apply2(BinAnd)(ReadRegister(a), Const(b)))
          case "borr" => JumpRelative(Apply2(BinOr)(ReadRegister(a), ReadRegister(b)))
          case "bori" => JumpRelative(Apply2(BinOr)(ReadRegister(a), Const(b)))
          case "setr" => JumpRelative(ReadRegister(a))
          case "seti" => JumpRelative(Const(a))
          case _ => throw new IllegalArgumentException("Unsupported setting IP with boolean opcodes") // technically possible. Have not seen
        }
      else
        opname match {
          case "addr" => SetWord(outputRegister, Apply2(Add)(ReadRegister(a), ReadRegister(b)))
          case "addi" => SetWord(outputRegister, Apply2(Add)(ReadRegister(a), Const(b)))
          case "mulr" => SetWord(outputRegister, Apply2(Mul)(ReadRegister(a), ReadRegister(b)))
          case "muli" => SetWord(outputRegister, Apply2(Mul)(ReadRegister(a), Const(b)))
          case "banr" => SetWord(outputRegister, Apply2(BinAnd)(ReadRegister(a), ReadRegister(b)))
          case "bani" => SetWord(outputRegister, Apply2(BinAnd)(ReadRegister(a), Const(b)))
          case "borr" => SetWord(outputRegister, Apply2(BinOr)(ReadRegister(a), ReadRegister(b)))
          case "bori" => SetWord(outputRegister, Apply2(BinOr)(ReadRegister(a), Const(b)))
          case "setr" => SetWord(outputRegister, ReadRegister(a))
          case "seti" => SetWord(outputRegister, Const(a))
          case "gtir" => SetBoolean(outputRegister, Apply2(Greater)(Const(a), ReadRegister(b)))
          case "gtri" => SetBoolean(outputRegister, Apply2(Greater)(ReadRegister(a), Const(b)))
          case "gtrr" => SetBoolean(outputRegister, Apply2(Greater)(ReadRegister(a), ReadRegister(b)))
          case "eqir" => SetBoolean(outputRegister, Apply2(EqualsTo)(Const(a), ReadRegister(b)))
          case "eqri" => SetBoolean(outputRegister, Apply2(EqualsTo)(ReadRegister(a), Const(b)))
          case "eqrr" => SetBoolean(outputRegister, Apply2(EqualsTo)(ReadRegister(a), ReadRegister(b)))
        }
  }

  def convertProgram(ipSelector: IpSelector)(program: Seq[OperationBinary]): Seq[LabelledBinaryStatement] =
    program.zipWithIndex.map{ case (ob, i) => LabelledBinaryStatement(i, convert(ipSelector)(ob)) }

  def showExpression[T](e: Expression[T]): String = e match {
    case Add => "+"
    case Mul => "*"
    case Div => "/"
    case BinAnd => "&"
    case BinOr => "|"
    case Greater => ">"
    case EqualsTo => "=="
    case ReadRegister(reg: RegisterId) => "r" + reg
    case ReadBoolean(reg: RegisterId) => "(r" + reg + " == 1)"
    case Const(value) => "" + value
    case a: Apply2[_,_,_] => "(" + showExpression(a.a) + " " + showExpression(a.op) + " " + showExpression(a.b) + ")"
  }

  def showStatement(b: BinaryStatement): String = b match {
    case SetWord(registerId, expression) => s"r($registerId) := " + showExpression(expression)
    case SetBoolean(registerId, expression) =>  s"r($registerId) := " + showExpression(expression)
    case JumpRelative(expression) => "jump " + showExpression(expression) + " + 1:"
    case Jump(pos: IP) => "jump " + (pos + 1) + ":"
  }

  type AdvancedProgram = List[LabelledBinaryStatement] // List is better for pattern matching

  def showProgram(p: AdvancedProgram): String =
    p
      .map{ case LabelledBinaryStatement(i, s) => ("" + i).reverse.padTo(2, '0').reverse + ": " + showStatement(s)}
      .mkString("\n")

  implicit val rewritableExpression: RewritableTree[Expression0] = new RewritableTree[Expression0] {
    override def children(node: Expression0): List[Expression0] = node match {
      case a: Apply2[_,_,_] => List(a.a, a.op, a.b)
      case _ => List()
    }

    override def replaceChildren(node: Expression0, newChildren: List[Expression0]): Expression0 = {
      def h = newChildren.head
      def h1 = newChildren.tail.head
      def h2 = newChildren.tail.tail.head
      node match {
        case _: Apply2[_,_,_] => Apply2(h1.asInstanceOf[Expression[(Any, Any) => Any]])(h.asInstanceOf[Expression[Any]],h2.asInstanceOf[Expression[Any]])
        case _ => node
      }
    }
  }

  implicit class Expression0Ops(e: Expression0){
    def contains(other: Expression0): Boolean = {
      val seq: Seq[Expression0] =
        RewritableTree.collectWidthFirst[Expression0, Expression0, Seq[Expression0]]{
          case e1: Expression0 if e1 == other => e1
        }.apply(e)
      seq.nonEmpty
    }

    def replace(partialFunction: PartialFunction[Expression0, Expression0]): Expression0 = {
      RewritableTree.rewriteOnce[Expression0](partialFunction).apply(e)
    }

    def replaceReadRegister(registerId: RegisterId, another: Expression0): Expression0 =
      replace{
        case ReadRegister(r) if r == registerId => another
      }

  }
  def substituteExpressions(p: AdvancedProgram, previous: List[LabelledBinaryStatement] = Nil): AdvancedProgram = p match {
    case Nil => previous.reverse
    case LabelledBinaryStatement(ip1, SetWord(r, e1)) ::
         LabelledBinaryStatement(ip2, SetWord(rr, e2)) :: t if r == rr =>
      val expression: Expression0 = e2.replaceReadRegister(r, e1)

      substituteExpressions(LabelledBinaryStatement(ip1, SetWord(rr, expression.asInstanceOf[Expression[Word]])) :: t, previous)
    case LabelledBinaryStatement(ip1, SetWord(r, e1)) ::
         LabelledBinaryStatement(ip2, SetBoolean(rr, e2)) :: t if r == rr =>
      val expression: Expression0 = e2.replaceReadRegister(r, e1)

      substituteExpressions(LabelledBinaryStatement(ip1, SetBoolean(rr, expression.asInstanceOf[Expression[Boolean]])) :: t, previous)
    case LabelledBinaryStatement(ip1, SetWord(r1, e1)) ::
         LabelledBinaryStatement(ip2, SetWord(r2, e2)) ::
         LabelledBinaryStatement(ip3, SetWord(r3, e3)) :: t if r1 == r3 && !e2.contains(ReadRegister(r1)) =>
      val expression: Expression0 = e3.replaceReadRegister(r1, e1)

      substituteExpressions(
        LabelledBinaryStatement(ip1, SetWord(r2, e2)) ::
        LabelledBinaryStatement(ip1 + 1, SetWord(r3, expression.asInstanceOf[Expression[Word]])
        ) :: t, previous)

    case h :: t =>
      substituteExpressions(t, h :: previous)
  }
//
//  sealed trait ExpandedMicroCode[ResultType]
//  case class OriginalMicroCode[T](microCode: OperationMicroCode) extends ExpandedMicroCode[T]
//  case class Block[T](seq: Seq[ExpandedMicroCode[T]]) extends ExpandedMicroCode[T]
//  case class DoWhileLoop[T](block: ExpandedMicroCode[T], condition: ExpandedMicroCode[Boolean]) extends ExpandedMicroCode[T]
////  case class Jump[T](pos: Int) extends ExpandedMicroCode[T]
//  case class Div[T](pos: Int) extends ExpandedMicroCode[Word]
//  case class If[T](condition: ExpandedMicroCode[Boolean]) extends ExpandedMicroCode[T]

//  case class Invocation2[T](microCode: ExpandedMicroCode[T], arg1: ExpandedMicroCode[T], arg2: ExpandedMicroCode[T]) extends ExpandedMicroCode[T]
//  case class Invocation1(microCode: ExpandedMicroCode[T], arg1: ExpandedMicroCode[Word]) extends ExpandedMicroCode[Unit]

  def optimize(prog: AdvancedProgram): AdvancedProgram =
    SequenceUtils.findFixedPoint(prog)(substituteExpressions(_))
  def main(args: Array[String]): Unit = {
    val p = parseProgramSpecification(Day21.lines)
    val prog: AdvancedProgram = convertProgram(p.ipSelector)(p.instructions).toList
    println(showProgram(prog))
    println()
    println(showProgram(optimize(prog)))
  }
}
