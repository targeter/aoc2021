package aoc2021

import shared._
import shared.ops.StringOps

import scala.annotation.tailrec

case object Day16 extends AocTools(day = 16) {

  val MIN_PACKET_LENGTH = 11

  def hexLookup = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  case class Header(version: Int, typeId: Int)

  def hex2bin(input: String): String = input.map(hexLookup).mkString

  trait Packet {
    def header: Header
    def value: Long
    def versionSum: Int
  }

  case class LiteralPacket(header: Header, value: Long) extends Packet {
    override val versionSum: Int = header.version
  }

  case class OperatorPacket(val header: Header, subPackets: List[Packet], op: List[Long] => Long) extends Packet {
    override val versionSum: Int = header.version + subPackets.map(_.versionSum).sum

    override def value: Long = op(subPackets.map(_.value))
  }

  object OperatorPacket {
    def apply(header: Header, subPackets: List[Packet]): OperatorPacket = header.typeId match {
      case 0 => OperatorPacket(header, subPackets, _.sum)
      case 1 => OperatorPacket(header, subPackets, _.product)
      case 2 => OperatorPacket(header, subPackets, _.min)
      case 3 => OperatorPacket(header, subPackets, _.max)
      case 5 => OperatorPacket(header, subPackets, { case List(a, b) => if (a > b) 1 else 0 })
      case 6 => OperatorPacket(header, subPackets, { case List(a, b) => if (a < b) 1 else 0 })
      case 7 => OperatorPacket(header, subPackets, { case List(a, b) => if (a == b) 1 else 0 })
      case x => sys.error("wrong type " + x)
    }
  }

  def parseHeader(input: String): (Header, String) = {
    val (header, rest) = input.splitAt(6)
    val (version, typeId) = header.splitAt(3)
    Header(version.toBinInt, typeId.toBinInt) -> rest
  }

  def parseNextLiteral(header: Header, str: String): (LiteralPacket, String) = {
    val bitGroup = str.grouped(5).toList
    val lastValIndex = bitGroup.indexWhere(_.head == '0')
    val (packet, rest) = bitGroup.splitAt(lastValIndex + 1)
    val packStr = packet.map(_.tail).mkString
    val packInt = packStr.toBinLong

    LiteralPacket(header, packInt) -> rest.mkString
  }

  def parseNextOperator(header: Header, str: String): (OperatorPacket, String) = {
    if (str.head == '1') {
      val (lengthStr, packetRest) = str.tail.splitAt(11)
      val (subPackets, rest) = parseNumberOfPackets(packetRest, lengthStr.toBinInt)
      OperatorPacket(header, subPackets) -> rest
    } else {
      val (lengthStr, packetRest) = str.tail.splitAt(15)
      val length = lengthStr.toBinInt
      val packet = packetRest.take(length)
      val subPackets = parseAllPackets(packet)
      OperatorPacket(header, subPackets) -> packetRest.drop(length)
    }

  }

  def parseNumberOfPackets(str: String, n: Int): (List[Packet], String) = {
    if (n == 0) (Nil, str)
    else {
      val (packet, packetRest) = parsePacket(str)
      val (packets, rest) = parseNumberOfPackets(packetRest, n - 1)
      (packet :: packets, rest)
    }
  }

  def parseAllPackets(str: String): List[Packet] = {
    val (subPacket, packetRest: String) = parsePacket(str)
    val parsed = if (packetRest.length < MIN_PACKET_LENGTH) Nil else parseAllPackets(packetRest)
    subPacket :: parsed
  }

  def parsePacket(input: String): (Packet, String) = {
    val (header, rest) = parseHeader(input)
    val (packet: Packet, remaining) = header.typeId match {
      case 4 => parseNextLiteral(header, rest)
      case _ => parseNextOperator(header, rest)
    }
    (packet, remaining)
  }

  def step1: Int = step1(inputLines.head)

  def step1(input: String): Int = {
    val result = parseAllPackets(hex2bin(input))
    result.map(_.versionSum).sum
  }

  def step2: Long = step2(inputLines.head)

  def step2(input: String): Long = {
    val result: Seq[Packet] = parseAllPackets(hex2bin(input))
    result.head.value

  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
