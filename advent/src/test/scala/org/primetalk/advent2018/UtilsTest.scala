package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils.unfoldWithSuffix
import org.primetalk.advent.tools.Utils

class UtilsTest extends BaseTest {

  behavior of "UtilsTest"

  it should "intsRegex" in {
    new Utils {
      parseAllIntsInString("0981234lkj;lk123;lkj;1;l1;lkj4;kj a 999") shouldBe
        Seq(981234, 123, 1, 1, 4, 999)
    }
  }

  it should "unfold" in {
    new Utils {
      unfoldWithSuffix(List[Int]()){ i: Int =>
        if(i >= 10) None
        else Some((i + 1, i*i))
      }(1) shouldBe
        List(81, 64, 49, 36, 25, 16, 9, 4, 1)
    }
  }
}
