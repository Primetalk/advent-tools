package org.primetalk.advent3.tools

object LongDistribution:
  opaque type Distribution[T] = Map[T, Long]

  def lift[T](t: T): Distribution[T] = Map(t -> 1L)
  
  def fromList[T](list: List[(T, Long)]): Distribution[T] =
    list.groupMapReduce(_._1)(_._2)(_ + _)

  extension [T](d: Distribution[T])
    def toList: List[(T, Long)] = 
      d.toList

    def dmap[B](f: T => B): Distribution[B] =
      fromList(
        for
          (dd, ddl) <- d.toList
        yield
          (f(dd), ddl)
      )

    def dflatMap[B](f: T => Distribution[B]): Distribution[B] =
      fromList(
        for
          (dd, ddl) <- d.toList
          (bb, bbl) <- f(dd)
        yield
          (bb, ddl * bbl)
      )

    def prod[A](a: Distribution[A]): Distribution[(T, A)] =
      dzip(a).fold((_,_))

    def dzip[A](a: Distribution[A]): (Distribution[T], Distribution[A]) = 
      (d, a)

    def forall(p: T => Boolean): Boolean = 
      d.keys.forall(p)

  extension [A, B](p: (Distribution[A], Distribution[B]))
    def fold[C](f: (A, B) => C): Distribution[C] =
      p._1.dflatMap(a => p._2.dmap(b => f(a, b)))

  extension (d: Distribution[Int])
    def +(o: Distribution[Int]): Distribution[Int] =
      d.dzip(o).fold(_ + _)
    
    def *(n: Int): Distribution[Int] =
      if n == 1 then
        d
      else
        (d * (n - 1) + d)

    def %(m: Int): Distribution[Int] =
      d.dmap(_ % m)
