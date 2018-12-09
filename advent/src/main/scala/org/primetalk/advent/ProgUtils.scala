package org.primetalk.advent

trait ProgUtils {
  type State
  type Operation = State => State
  type Program = Stream[Operation]

  def infiniteProgram(prog: Stream[Operation]): Stream[Operation] =
    Stream.continually(prog).flatten

  def findFirst(program: Program, p: State => Boolean): State => Option[State] = initial =>
    program
      .scanLeft(initial){ case (s, op) => op(s) }
      .dropWhile(s => !p(s))
      .headOption

}
