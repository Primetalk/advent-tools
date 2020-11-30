package org.primetalk.advent2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fastparse._
import Day9._

class Day9Test extends AnyFlatSpec with Matchers {

  behavior of "Day9Test"
  /*
  Here are some self-contained pieces of garbage:

      <>, empty garbage.
      <random characters>, garbage containing random characters.
      <<<<>, because the extra < are ignored.
      <{!>}>, because the first > is canceled.
      <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
      <!!!>>, because the second ! and the first > are canceled.
      <{o"i!a,<{i<a>, which ends at the first >.


   */
  it should "garbage" in {
    parse("<>", garbage(_))
    parse("<random characters>", garbage(_))
    parse("<<<<>", garbage(_))
    parse("<{!>}>", garbage(_))
    parse("<!!>", garbage(_))
    parse("<!!!>>", garbage(_))
    parse("<{o\"i!a,<{i<a>", garbage(_))
  }
  /*
  Here are some examples of whole streams and the number of groups they contain:

      {}, 1 group.
      {{{}}}, 3 groups.
      {{},{}}, also 3 groups.
      {{{},{},{{}}}}, 6 groups.
      {<{},{},{{}}>}, 1 group (which itself contains garbage).
      {<a>,<a>,<a>,<a>}, 1 group.
      {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
      {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).

   */
//  it should "group" in {
//    score(parseSingleGroup("{}")) shouldBe 1
//    score(parseSingleGroup("{{{}}}")) shouldBe 3
//    score(parseSingleGroup("{{},{}}")) shouldBe 3
//    score(parseSingleGroup("{{{},{},{{}}}}")) shouldBe 6
//    score(parseSingleGroup("{<{},{},{{}}>}")) shouldBe 1
//    score(parseSingleGroup("{<a>,<a>,<a>,<a>}")) shouldBe 1
//    score(parseSingleGroup("{{<a>},{<a>},{<a>},{<a>}}")) shouldBe 5
//    score(parseSingleGroup("{{<!>},{<!>},{<!>},{<a>}}")) shouldBe 2
//  }

}
