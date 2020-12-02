package org.primetalk.advent2020

class Day2002Test extends BaseTest {

  import Day2002._

  "parse" should "parse test" in {
    parse("6-10 z: zhzzzzfzzzzzzzzzpzz") shouldBe Record(6, 10, 'z', "zhzzzzfzzzzzzzzzpzz")
  }

  "parse2" should "parse test" in {
    parse2("6-10 z: zhzzzzfzzzzzzzzzpzz") shouldBe Record(6, 10, 'z', "zhzzzzfzzzzzzzzzpzz")
  }

  behavior of "Day2002Test"


  it should "find pair" in {
    answer1 shouldBe 393
  }

  it should "find triple" in {
    answer2 shouldBe 690
  }

}
