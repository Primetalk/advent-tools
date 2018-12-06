package org.primetalk.advent

import org.primetalk.advent.Day2._
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {

  "examples" should "match" in {
    counts("abcdef") shouldBe Set(1)
    counts("bababc") shouldBe Set(1, 2, 3)
    counts("abbcde") shouldBe Set(1, 2)
    counts("abcccd") shouldBe Set(1, 3)
    counts("aabcdd") shouldBe Set(1, 2)
    counts("abcdee") shouldBe Set(1, 2)
    counts("ababab") shouldBe Set(3)
  }

}
