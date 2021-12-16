package org.primetalk.advent3.tools

trait BitsParser[T]:
  def parse(bits: List[Char]): (T, List[Char])

object BitsParser:
  extension [T](p: BitsParser[T])
    def map[A](f: T => A): BitsParser[A] = new BitsParser[A]:
      def parse(bits: List[Char]): (A, List[Char]) =
        val (t, rest) = p.parse(bits)
        (f(t), rest)

    def flatMap[A](f: T => BitsParser[A]): BitsParser[A] = new BitsParser[A]:
      def parse(bits: List[Char]): (A, List[Char]) =
        val (t, rest) = p.parse(bits)
        val pa = f(t)
        pa.parse(rest)

    def repN(n: Int): BitsParser[List[T]] = 
      if n == 1 then 
        p.map(_ :: Nil)
      else if n == 0 then 
        throw IllegalArgumentException("repN doesn't work with n == 0")
      else
        (p ~ repN(n - 1)).map(_ :: _)
    // repeats until there is something in the stream
    def repGreedy: BitsParser[List[T]] = new BitsParser[List[T]]:

      def parse(bits: List[Char]): (List[T], List[Char]) =
        val (el, tail) = p.parse(bits)
        if tail.isEmpty then
          (el :: Nil, Nil)
        else
          val (rec, tail2) = parse(tail)
          (el :: rec, Nil)

    def ~[A](pa: BitsParser[A]): BitsParser[(T, A)] = new BitsParser[(T, A)]:
      def parse(bits: List[Char]): ((T,A), List[Char]) =
        val (t, rest) = p.parse(bits)
        val (a, rest2) = pa.parse(rest)
        ((t, a), rest2)

  extension (p: BitsParser[List[Char]])
    def toNumber: BitsParser[Long] = 
      p.map(_.mkString)
        .map(s => BigInt(s, 2).toLong)

  def fixedWidth(len: Int): BitsParser[List[Char]] = new BitsParser[List[Char]]:
    def parse(bits: List[Char]): (List[Char], List[Char]) =
      (bits.take(len), bits.drop(len))

  def fixedWidthNumber(len: Int): BitsParser[Long] = 
    fixedWidth(len).toNumber

  def bit: BitsParser[Boolean] =
    fixedWidth(1).map(_ == List('1'))
