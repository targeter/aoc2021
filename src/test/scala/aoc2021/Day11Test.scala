package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day11Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day11._

  private val exampleList = List(
      "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526")


  test("step1") {
    step1(exampleList) must be(1656)
  }

  test("step2") {
    step2(exampleList) must be(0)
  }

}
