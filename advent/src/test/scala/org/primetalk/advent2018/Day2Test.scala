package org.primetalk.advent2018

import org.primetalk.advent2018.Day2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFlatSpec with Matchers {
/*
  * abcdef contains no letters that appear exactly two or three times.
  * bababc contains two a and three b, so it counts for both.
  * abbcde contains two b, but no letter appears exactly three times.
  * abcccd contains three c, but no letter appears exactly two times.
  * aabcdd contains two a and two d, but it only counts once.
  * abcdee contains two e.
  * ababab contains three a and three b, but it only counts once.

 */
  "examples" should "result in  3" in {
    counts("abcdef") shouldBe Set(1)
    counts("bababc") shouldBe Set(1, 2, 3)
    counts("abbcde") shouldBe Set(1, 2)
    counts("abcccd") shouldBe Set(1, 3)
    counts("aabcdd") shouldBe Set(1, 2)
    counts("abcdee") shouldBe Set(1, 2)
    counts("ababab") shouldBe Set(3)
  }

}
