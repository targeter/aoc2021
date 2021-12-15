package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day15Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day15._

  private val exampleList = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  )

  test("step1") {
    step1(exampleList) must be(40)
  }

  test("step2") {
    parse(exampleList).expand(5).print
    step2(exampleList) must be(315)
  }

}
