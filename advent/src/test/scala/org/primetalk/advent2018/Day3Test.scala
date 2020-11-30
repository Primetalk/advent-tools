package org.primetalk.advent2018

import org.primetalk.advent2018.Day2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3Test extends AnyFlatSpec with Matchers {

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
