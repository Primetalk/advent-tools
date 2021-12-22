package org.primetalk.advent3.tools

type InclusiveRange = (Int, Int) // [min,max]

extension (r: InclusiveRange)

  def isInside(x: Int): Boolean = 
    r._1 <= x && x <= r._2

  def intersect(other: InclusiveRange): Option[InclusiveRange] =
    if other._1 < r._1 then
      other.intersect(r)
    else 
      // other._1 >= r._1
      if other._1 <= r._2 then
        if isInside(other._2) then
          Some(other)
        else
          Some((other._1, r._2))
      else
        None
        
  /** 0..2 elements in the result. */
  def exclude(o: InclusiveRange): List[InclusiveRange] =
    List(
      if r._1 < o._1 then
        List((r._1, math.min(o._1 - 1,r._2)))
      else
        Nil
        ,
      if o._2 < r._2 then
        List((math.max(r._1, o._2 + 1), r._2))
      else
        Nil
    )
    .flatten
    .distinct
