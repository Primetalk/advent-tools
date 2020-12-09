package org.primetalk.advent.tools

class CollectionUtilsTest extends BaseTest {
  "partialSums" should "start in a known way" in {
    CollectionUtils.partialSums(List(1,2,3)) shouldBe List(1,3,6)
  }
}
