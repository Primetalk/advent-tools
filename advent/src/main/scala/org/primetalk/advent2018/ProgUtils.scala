package org.primetalk.advent2018

trait ProgUtils {
  type State
  type Operation = State => State
  type Program = LazyList[Operation]

  def infiniteProgram(prog: LazyList[Operation]): LazyList[Operation] =
    LazyList.continually(prog).flatten

  def findFirst(program: Program, p: State => Boolean): State => Option[State] = initial =>
    program
      .scanLeft(initial){ case (s, op) => op(s) }
      .dropWhile(s => !p(s))
      .headOption

}
