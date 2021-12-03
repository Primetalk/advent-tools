package org.primetalk.advent3.tools

case class BitString(bits: String):
  def toInt: Int = 
    Integer.parseInt(bits, 2)
  def bitAtPos(pos: Int): Boolean =
    bits.charAt(pos) == '1'
  
  override def toString = bits

object BitString:
  def fromBinaryString(s: String) = BitString(s)
