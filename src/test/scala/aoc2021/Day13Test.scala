package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day13Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day13._

  private val exampleList = List(
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5"
  )

  test("step1") {
    val (grid, instructions) = parse(exampleList)
    printGrid(grid)
//    println("")
    val (a, b) = grid.splitAt(7)
//    printGrid(a)
//    println("--")
//    printGrid(b.tail)
//
//    println("")
//    printGrid((instructions.head.fold(grid)))

    val grid1 = instructions.head.fold(grid)
    grid1.flatten.count(_ == DOT) must be(17)
    val grid2 = instructions.last.fold(grid1)
    printGrid(grid1)
    println("-----")
    printGrid(grid2)
    grid1.flatten.count(_ == DOT) must be(17)
  }

  test("step2") {
    step2(exampleList) must be(())
  }

}
