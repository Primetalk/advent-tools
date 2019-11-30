package org.primetalk.advent2018

import org.primetalk.advent.tools.Geom2dUtils.{Down => DisplayDown, Up => DisplayUp, _}
import org.primetalk.advent.tools.SequenceUtils.unfold
import org.primetalk.advent.tools.Utils

object Day13 extends Utils {

  lazy val inputTextFromResource : Iterator[String] =
    readResource("day13.txt")

  lazy val lines: IndexedSeq[String] =
    inputTextFromResource.toIndexedSeq

  def size(lines: IndexedSeq[String]): Vector2d = (lines.map(_.length).max, lines.length)

  /** @param phase - 0 - left, 1 - straight, 2 - right*/
  case class CartState(n: Int, position: Position, direction: Direction, phase: Int = 0)

  type CartStates = IndexedSeq[CartState]

  val MapUp: Direction = DisplayDown
  val MapDown: Direction = DisplayUp
  val cartDirections: Map[Char, Direction] = Map(
    '^' -> MapUp,
    'v' -> MapDown,
    '<' -> Left,
    '>' -> Right
  )

  def carts(lines: IndexedSeq[String]): CartStates = {
    val size1 = size(lines)
    def go(i: Int, j: Int, k: Int, found: List[CartState]): CartStates = {
      if(j >= size1._2)
        found.reverse.toIndexedSeq
      else {
        if(i >= lines(j).length)
          go(0, j + 1, k, found)
        else {
          val ch = lines(j).charAt(i)
          if(cartDirections.keySet.contains(ch))
            go(i + 1, j, k + 1, CartState(k, (i, j), cartDirections(ch)) :: found)
          else
            go(i + 1, j, k, found)
        }
      }
    }
    go(0, 0, 0, List())
  }

  def toDisplayWithoutCarts(lines: IndexedSeq[String], carts: CartStates): Display[Char] = {
    val size1 = size(lines)
    val display = Display[Char](origin, size1)()
    lines.zipWithIndex.foreach {
      case (line, j) =>
        val padding = "".padTo(size1._1 - line.length, ' ')
        display.array(j) = (line + padding).toCharArray
    }
    val display2 = display//.transpose
    for {
      CartState(_, pos, dir, _) <- carts
      //backPosition = pos + (0, 1)
    } {
      display2(pos) = dir match {
        case MapUp | MapDown => '|'
        case Left|Right => '-'
        case _ => '@'
      }
    }
    display2
  }

  def show(display: Display[Char]): Unit = {
    display.array.foreach(arr => println(new String(arr)))
  }

  def showWithCarts(display: Display[Char], carts: CartStates): Unit = {
    val mapByY = carts.map(c => (c.position._2, c))
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
    for{
      y <- display.ys
    } {
      val line = display.array(y).toVector.toArray
      val cartsOnLine = mapByY.getOrElse(y, Set())
      cartsOnLine.foreach(c => line(c.position._1) = '#')
      println(new String(line))
    }
  }

  def nextDirectionOnJunction(dir: Direction, phase: Int): Direction = phase match {
    case 1 => // Straight
      dir
    case 0 => // Left
      dir.rotate((0, -1))
    case 2 => // Right
      dir.rotate((0, 1))
  }

  def moveCart(display: Display[Char])(cart: CartState): CartState = cart match {
    case CartState(i, position, direction, phase) =>
      val nextPos = position + direction
      val mapChar = display(nextPos)
      val nextDir = (mapChar, direction) match {
        case ('-' , _) => direction
        case ('|' , _) => direction
        case ('/' , MapUp)   => Right
        case ('/' , MapDown) => Left
        case ('/' , Left)    => MapDown
        case ('/' , Right)   => MapUp
        case ('\\', MapUp)   => Left
        case ('\\', MapDown) => Right
        case ('\\', Left)    => MapUp
        case ('\\', Right)   => MapDown
        case ('+' , _) =>
          nextDirectionOnJunction(direction, phase)
        case _ =>
          direction
      }
      val nextPhase = mapChar match {
        case '+' => (phase + 1) % 3
        case _ => phase
      }
      CartState(i, nextPos, nextDir, nextPhase)
  }

  def tick(display: Display[Char])(cartStates: CartStates): Either[Position, CartStates] = {
    implicit val ordering: Ordering[Position] = Ordering.by(p => p._2 * display.size._1 + p._1)
    val sortedCarts: Vector[CartState] = cartStates.sortBy(_.position).toVector
    val initialPositions = sortedCarts.map(_.position).toSet
    def go(k: Int, movedCarts: List[CartState], positions: Set[Position]): Either[Position, CartStates] = {
      if(k == sortedCarts.length)
        scala.Right(movedCarts.sortBy(_.n).toIndexedSeq)
      else {
        val cart = sortedCarts(k)
        val moved = moveCart(display)(cart)
        val otherPositions = positions - cart.position
        if(otherPositions.contains(moved.position))
          scala.Left(moved.position)
        else
          go(k + 1, moved :: movedCarts, otherPositions + moved.position)
      }
    }
    go(0, Nil, initialPositions)
  }
  // 64,33
  // 15,100
  // 113,135
  // 69,46
  lazy val answer1: Long = {
    val cartStates: CartStates = carts(lines)
    val len = cartStates.length
    val display = toDisplayWithoutCarts(lines, cartStates)
    show(display)
    var count = 0
    unfold(cartStates){cs =>
      val next: Either[(Int, Int), CartStates] = tick(display)(cs)

      count += 1
      next match {
        case scala.Left(position) =>
          println(position)
          None
        case scala.Right(n) =>
          println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ " + count)
          showWithCarts(display, n)
          Some(n)
      }
    }
    len
  }

  // Part 2


  def tick2(display: Display[Char])(cartStates: CartStates): (Set[CartState], CartStates) = {
    implicit val ordering: Ordering[Position] = Ordering.by(p => p._2 * display.size._1 + p._1)
    val sortedCarts: Vector[CartState] = cartStates.sortBy(_.position).toVector
    val initialPositions = sortedCarts.map(_.position).toSet
    def go(k: Int, movedCarts: List[CartState], positions: Set[Position], collisions: Set[CartState]): (Set[CartState], CartStates) = {
      if(k == sortedCarts.length)
        (collisions, movedCarts.sortBy(_.n).toIndexedSeq)
      else {
        val cart = sortedCarts(k)
        val moved = moveCart(display)(cart)
        val otherPositions = positions - cart.position
        val newCollisions = if(otherPositions.contains(moved.position)) {
          Set(moved) ++
            (movedCarts ++ sortedCarts.drop(k)).find(_.position == moved.position)

        }
        else
        Set()
          go(k + 1, moved :: movedCarts, otherPositions + moved.position, collisions ++ newCollisions)
      }
    }
    go(0, Nil, initialPositions, Set())
  }


  lazy val answer2: Long = {
    val cartStates: CartStates = carts(lines)
    val len = cartStates.length
    val display = toDisplayWithoutCarts(lines, cartStates)
    show(display)
    var count = 0
    unfold(cartStates){cs =>
      val (collisions, csNext) = tick2(display)(cs)
      val ids = collisions.map(_.n)
      val next = csNext.filterNot(c => ids.contains(c.n))
      count += 1
      if(next.size == 1) {
        println(next.head.position)
        None
      } else {
          println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ " + count)
//          showWithCarts(display, next)
          Some(next)
      }
    }
    count
  }

  def main(args: Array[String]): Unit = {
//    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
