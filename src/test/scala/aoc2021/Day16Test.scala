package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day16Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day16._

  test("hex2bin") {
    hex2bin("D2FE28") must be("110100101111111000101000")
  }

  test("parsePacket") {
    val (packet, rest) = parsePacket(hex2bin("D2FE28"))
    packet must be(LiteralPacket(Header(6, 4), 2021))
    rest must be("000")
  }

  test("nextOperator") {
    val str = hex2bin("38006F45291200")
    parsePacket(str)._1 must be(
      OperatorPacket(
        Header(1, 6),
        List(
          LiteralPacket(Header(6, 4), 10),
          LiteralPacket(Header(2, 4), 20)
        )
      )
    )

    val str2 = hex2bin("EE00D40C823060")
    parsePacket(str2)._1.asInstanceOf[OperatorPacket].subPackets.map(_.value) must be(List(1, 2, 3))
    parsePacket(str2)._1.asInstanceOf[OperatorPacket].value must be(3) // Max operator
  }

  test("step1") {
    step1("8A004A801A8002F478") must be(16)
    step1("620080001611562C8802118E34") must be(12)
    step1("C0015000016115A2E0802F182340") must be(23)
    step1("A0016C880162017C3686B18A3D4780") must be(31)
  }

  test("step2") {
    step2("C200B40A82") must be(3)
    step2("04005AC33890") must be(54)
    step2("880086C3E88112") must be(7)
    step2("CE00C43D881120") must be(9)
    step2("D8005AC2A8F0") must be(1)
    step2("F600BC2D8F") must be(0)
    step2("9C005AC2F8F0") must be(0)
    step2("9C0141080250320F1802104A08") must be(1)
  }

}
