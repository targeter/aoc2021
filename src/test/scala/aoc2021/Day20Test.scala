package aoc2021

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day20Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2021.Day20._

  val exampleProgram = """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
""".replaceAll("\\n", "")

  private val exampleList =
    s"""$exampleProgram
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin.split("\\n").toList

  test("step1") {
    step1(exampleList) must be(35)
  }

  test("step2") {
    step2(exampleList) must be(0)
  }

}
