package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day05Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day05._

  private val exampleList = List(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )

  test("line points") {
    Line(Point(1,1), Point(1,3)).points must contain theSameElementsAs Seq(Point(1,1), Point(1,2), Point(1,3))
    Line(Point(1,3), Point(1,1)).points must contain theSameElementsAs Seq(Point(1,1), Point(1,2), Point(1,3))
    Line(Point(1,1), Point(3,3)).points must contain theSameElementsAs Seq(Point(1,1), Point(2,2), Point(3,3))
    Line(Point(3,3), Point(1,1)).points must contain theSameElementsAs Seq(Point(1,1), Point(2,2), Point(3,3))
  }



  test("step1") {
    parse(exampleList).last must be(Line(Point(5,5), Point(8,2)))

    step1(exampleList) must be(5)
  }

  test("step2") {
    step2(exampleList) must be(12)
  }

}
