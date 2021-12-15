package org.primetalk.advent3.tools

import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._

class HeapSpec extends UnitSpec:
  "Heap" should "return minimal element" in {
    val h1 = Heap.empty.add(12)
    h1.getMin should equal (Some(12))
  }
