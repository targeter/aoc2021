package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day09Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day09._

  private val exampleList = List(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  )

  test("step1") {
    step1(exampleList) must be(15)
  }

  test("step2") {
    step2(exampleList) must be(1134)
  }

}
