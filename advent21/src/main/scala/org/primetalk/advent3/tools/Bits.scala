package org.primetalk.advent3.tools

case class Bits(repr: BigInt, width: Int):

  def toInt = 
    repr.toInt

  def bitAtPosLSF(pos: Int): Boolean = 
    // if pos > width then throw new IllegalArgumentException("pos is too large")
    ((repr>>pos) & Bits.one.repr) == Bits.one.repr

  def bitAtPosMSF(pos: Int): Boolean = 
    bitAtPosLSF(width - 1 - pos)

  override def toString = 
    val s = repr.toString(2)
    val diff = width - s.length
    if diff > 0 then 
      "".padTo(diff, '0') + s 
    else 
      s

  def invert: Bits = 
    Bits(repr ^ upper.repr, width)

  def upper: Bits = 
    Bits(BigInt(2).pow(width) - 1, width)

  def lower: Bits = 
    Bits(BigInt(0), width)

object Bits:
  val one = Bits(BigInt(1), 1)
  def fromBinaryString(s: String): Bits = 
    Bits(BigInt(s, 2), s.length)
  def fromSeqMSF(s: Seq[Boolean]): Bits = 
    fromBinaryString(s.map(f => if f then '1' else '0').mkString)
  def fromSeqLSF(s: Seq[Boolean]): Bits = 
    fromBinaryString(s.reverse.map(f => if f then '1' else '0').mkString)
