package org.primetalk.ref

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

def slice(n: Int Refined Positive): [A] => List[A] => List[List[A]] =
  [a] => (lst: List[a]) => 
    val (init, tail) = lst.splitAt(n)
    if tail.isEmpty then
      List(init)
    else
      init :: slice(n)(tail)
