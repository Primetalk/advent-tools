package org.primetalk.advent3.tools

object LinearAlgebra:
  type Row = (List[BigInt], BigInt)
  type SystemOfEquations = List[Row]

  extension (r: Row)
    def *(k: BigInt): Row =
      (r._1.map(_ * k), r._2*k)
    def --(other: Row): Row =
      (r._1.zip(other._1).map(_ - _),
        r._2 - other._2)
  def argmax(l: List[BigInt]): Int =
    l.zipWithIndex.maxBy(_._1)._2

  def swap[A](l: List[A], i: Int, j: Int): List[A] =
    if i > j then
      swap(l, j, i)
    else if i == j then
      l
    else
      val li = l(i)
      val lj = l(j)

      val res = l.take(i-1) ::: lj :: l.slice(i + 1, j) ::: li :: l.drop(j+1)
      require(res.size == l.size)
      res

  /** https://en.wikipedia.org/wiki/Gaussian_elimination
    *
  h := 1 /* Initialization of the pivot row */
  k := 1 /* Initialization of the pivot column */

  while h ≤ m and k ≤ n
    /* Find the k-th pivot: */
    i_max := argmax (i = h ... m, abs(A[i, k]))
    if A[i_max, k] = 0
        /* No pivot in this column, pass to next column */
        k := k + 1
    else
        swap rows(h, i_max)
        /* Do for all rows below pivot: */
        for i = h + 1 ... m:
            f := A[i, k] / A[h, k]
            /* Fill with zeros the lower part of pivot column: */
            A[i, k] := 0
            /* Do for all remaining elements in current row: */
            for j = k + 1 ... n:
                A[i, j] := A[i, j] - A[h, j] * f
        /* Increase pivot row and column */
        h := h + 1
        k := k + 1
    *  */
  def gauss(m: SystemOfEquations, k: Int = 0, passed: SystemOfEquations = Nil): SystemOfEquations =
    m match
      case Nil =>
        passed
      case head :: Nil =>
        head :: passed
      case _ =>
        val i_max = argmax(m.map(_._1(k).abs))
        val ms = swap(m, 0, i_max)
        val a = ms.head._1(k)
        val first = ms.head
        val newM = ms.tail.map{ r =>
          val b = r._1(k)
          if b == 0 then
            r
          else
            r * a -- first * b
        }
        gauss(newM, k + 1, first :: passed)

  def showSystemOfEquations(s: SystemOfEquations): String =
    val m = s.map(_._1)
    val ms = m.map(l => l.mkString("(", ", ", ")")).mkString("(",", ", ")")
    val v = s.map(_._2)
    val vs = v.mkString("(", ", ", ")")
    s"$ms * X = $vs"
end LinearAlgebra