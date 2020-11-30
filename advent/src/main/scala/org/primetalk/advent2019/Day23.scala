package org.primetalk.advent2019

import org.primetalk.advent.tools.Utils

/**
  *
  * https://adventofcode.com/2019/day/23
  *
  * --- Day 23: Category Six ---
  *
  * The droids have finished repairing as much of the ship as they can. Their report indicates that this was a Category 6 disaster - not because it was that bad, but because it destroyed the stockpile of Category 6 network cables as well as most of the ship's network infrastructure.
  *
  * You'll need to rebuild the network from scratch.
  *
  * The computers on the network are standard Intcode computers that communicate by sending packets to each other. There are 50 of them in total, each running a copy of the same Network Interface Controller (NIC) software (your puzzle input). The computers have network addresses 0 through 49; when each computer boots up, it will request its network address via a single input instruction. Be sure to give each computer a unique network address.
  *
  * Once a computer has received its network address, it will begin doing work and communicating over the network by sending and receiving packets. All packets contain two values named X and Y. Packets sent to a computer are queued by the recipient and read in the order they are received.
  *
  * To send a packet to another computer, the NIC will use three output instructions that provide the destination address of the packet followed by its X and Y values. For example, three output instructions that provide the values 10, 20, 30 would send a packet with X=20 and Y=30 to the computer with address 10.
  *
  * To receive a packet from another computer, the NIC will use an input instruction. If the incoming packet queue is empty, provide -1. Otherwise, provide the X value of the next packet; the computer will then use a second input instruction to receive the Y value for the same packet. Once both values of the packet are read in this way, the packet is removed from the queue.
  *
  * Note that these input and output instructions never block. Specifically, output instructions do not wait for the sent packet to be received - the computer might send multiple packets before receiving any. Similarly, input instructions do not wait for a packet to arrive - if no packet is waiting, input instructions should receive -1.
  *
  * Boot up all 50 computers and attach them to your network. What is the Y value of the first packet sent to address 255?
  *
  * Your puzzle answer was 21089.
  * --- Part Two ---
  *
  * Packets sent to address 255 are handled by a device called a NAT (Not Always Transmitting). The NAT is responsible for managing power consumption of the network by blocking certain packets and watching for idle periods in the computers.
  *
  * If a packet would be sent to address 255, the NAT receives it instead. The NAT remembers only the last packet it receives; that is, the data in each packet it receives overwrites the NAT's packet memory with the new packet's X and Y values.
  *
  * The NAT also monitors all computers on the network. If all computers have empty incoming packet queues and are continuously trying to receive packets without sending packets, the network is considered idle.
  *
  * Once the network is idle, the NAT sends only the last packet it received to address 0; this will cause the computers on the network to resume activity. In this way, the NAT can throttle power consumption of the network when the ship needs power in other areas.
  *
  * Monitor packets released to the computer at address 0 by the NAT. What is the first Y value delivered by the NAT to the computer at address 0 twice in a row?
  *
  * Your puzzle answer was 16658.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day23 extends Utils {

  lazy val inputTextFromResource : String =
    readResourceAsString("day23.txt")

  val program: Seq[Word] = inputTextFromResource.split(",").map(_.toLong).toSeq

  type Word = Long
  case class Packet(address: Word, X: Word, Y: Word)
  type Packets = List[Packet]
  class SingleComputer(val address: Word) extends IntCodeComputer9 {
    private var st: State = State(0, 0, new SimpleMemory[Day23.Word](program), address :: Nil, Nil)
    def runUntilInputIsEmptyOrPacket(queue: List[Packet]): List[Packet] = {
      require(queue.forall(_.address == address), "Invalid address on incoming packet")
      val xys = queue.flatMap{ p => List(p.X, p.Y)}
      if(xys.nonEmpty) {
        st = st.copy(inputs = st.inputs ::: xys)
      }

      val s0 =
        if(st.inputs.isEmpty) {
          st.copy(inputs = -1 :: Nil)
        } else
          st
      val s1 = runUntilOrFinish(s => s.inputs.isEmpty || s.outputs.size == 3)(s0)
      s1.outputs match {
        case Nil =>
          st = s1
          List()
        case y :: x :: addr :: t =>
          st = s1.copy(outputs = t)
          List(Packet(addr, x, y))
        case other =>
          throw new IllegalArgumentException(s"$other")
      }
    }
  }

  case class Network(computers: List[SingleComputer]) {
    var natPacket: Option[Packet] = None
    def runUntilNewPackets(flyingPackets: List[Packet]): List[Packet] = {
      computers.flatMap { computer =>
        val queue = flyingPackets.filter(_.address == computer.address)
        computer.runUntilInputIsEmptyOrPacket(queue)
      }
    }

    def runUntil255OrEmpty(flyingPackets: List[Packet]): (Packet, List[Packet]) = {
      @scala.annotation.tailrec
      def loop(flyingPackets: List[Packet], countNumberOfTimesItWasEmpty: Int = 0): (Packet, List[Packet]) = {
        flyingPackets.filter(_.address == 255) match {
          case List(p) =>
            natPacket = Some(p)
            (p, flyingPackets)
          case Nil =>

            val packets = runUntilNewPackets(flyingPackets)

            if(packets.nonEmpty)
              loop(packets)
            else {
              if (countNumberOfTimesItWasEmpty >= 50) // we only send NAT packet when we've been idle at least 10 times
                loop(natPacket match {
                  case Some(p) =>
//                                        natPacket = None
                    val packet0 = p.copy(address = 0L)
                    println(packet0.Y)
                    List(packet0)
                  case None =>

                    println("No more packets in the network")
                    Nil
                }
                )
              else
                loop(Nil, countNumberOfTimesItWasEmpty + 1)
            }

          case lst =>
            throw new IllegalStateException(s"Received more than one packet to 255 at once $lst")
        }
      }
      loop(
        flyingPackets match {
          case Nil =>
            natPacket match {
              case Some(p) =>
//                natPacket = None
                List(p.copy(address = 0L))
              case None =>
                Nil
            }
          case _ =>
            flyingPackets.filterNot(_.address == 255)
        }
      )
    }
  }

  def createNetwork: Network = {
    Network((0 until 50).
      map{ i => new SingleComputer(i)}.
      toList
    )
  }

  lazy val answer1: Long = {
    val n = createNetwork
    n.runUntil255OrEmpty(Nil)._1.Y
  }

  // Part 2

  lazy val answer2: Word = {
    val n = createNetwork
    @scala.annotation.tailrec
    def loopUntilDuplicateY(flyingPackets: List[Packet] = Nil, previousY: Word = 0L): Word = {
      val (p, rest) = n.runUntil255OrEmpty(flyingPackets)
//      println(p.Y)
//      if(p.Y == previousY)
//        p.Y
//      else
        loopUntilDuplicateY(rest, p.Y)
    }
    loopUntilDuplicateY()
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1) // 21089
    println("Answer2: " + answer2) // 26390 16667 16665 16663 16658
  }
}
