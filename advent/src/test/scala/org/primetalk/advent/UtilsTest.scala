package org.primetalk.advent

import org.scalatest.{FlatSpec, Matchers}

class UtilsTest extends FlatSpec with Matchers {

  behavior of "UtilsTest"

  it should "intsRegex" in {
    new Utils {
      parseAllIntsInString("0981234lkj;lk123;lkj;1;l1;lkj4;kj a 999") shouldBe
        Seq(981234, 123, 1, 1, 4, 999)
    }
  }

  it should "unfold" in {
    new Utils {
      unfoldWithSuffix(List[Int](2)){i => if(i >= 100) None else Some(i * i)}(2) shouldBe
        List(256, 16, 4, 2)
    }
  }
}
