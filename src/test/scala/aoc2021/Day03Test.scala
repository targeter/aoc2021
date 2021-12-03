package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day03Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day03._
  private val exampleList = List(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
  )

  test("step1") {
    findMostLeast(columns(exampleList)) must be((22,9))
    step1(exampleList) must be(198)
  }

  test("step2") {
    reduceMost(exampleList) must be(23)
    reduceLeast(exampleList) must be(10)
    step2(exampleList) must be(230)

  }

}
