package org.primetalk.advent3.tools

class BitsSpec extends UnitSpec:

  "Bits" should "invert known examples" in {
    val examples = List("10101" -> "01010", "11111" -> "00000", "001" -> "110")
    forAll(examples){ case (a, b) => 
      assert(Bits.fromBinaryString(a).invert === Bits.fromBinaryString(b))
      assert(Bits.fromBinaryString(b).invert === Bits.fromBinaryString(a))
    }
  }
