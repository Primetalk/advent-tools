package org.primetalk.advent

import ParsingUtils._
import fastparse._
import NoWhitespace._

class ParsingUtilsTest extends BaseTest {

  behavior of "ParsingUtilsTest"

  it should "integer" in {
    def i[_: P]: P[Int] =
      P(integer)
    parse("-123", i(_) ).get.value shouldBe -123
  }

}
