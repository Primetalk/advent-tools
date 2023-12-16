package org.primetalk.advent3.tools

class BitsSpec extends UnitSpec:

  "Bits" should "invert known examples" in {
    val examples = List("10101" -> "01010", "11111" -> "00000", "001" -> "110")
    forAll(examples){ case (a, b) => 
      assert(Bits.fromBinaryString(a).invert === Bits.fromBinaryString(b))
      assert(Bits.fromBinaryString(b).invert === Bits.fromBinaryString(a))
    }
  }

  "Bits" should "count known examples" in {
    val examples = List(1L -> 1, 3L -> 2, 32L -> 1, 48L -> 2, -1L -> 64)
    forAll(examples) { case (a, b) =>
      assert(Bits.countBits(a) === b)
    }
  }
