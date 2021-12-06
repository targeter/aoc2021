package aoc2021

import shared._

import scala.util.matching.Regex

case object Day05 extends AocTools(day = 5) {

  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {
    val diagonal: Boolean = start.x != end.x && start.y != end.y

    val points: Seq[Point] = {
      val xDiff = end.x.compare(start.x)
      val yDiff = end.y.compare(start.y)

      start +: List.unfold(start) { (last) =>
        val next = Point(last.x + xDiff, last.y + yDiff)
        if (last == end) None
        else Some(next, next)
      }
    }
  }

  def parse(input: Seq[String]): Seq[Line] = {
    val LinePattern: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
    input.collect { case LinePattern(x1, y1, x2, y2) =>
      Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }
  }

  def step1: Int = step1(inputLines)
  def step1(input: List[String]): Int = {
    val ventLines = parse(input).filterNot(_.diagonal)
    val lineMap = buildMap(ventLines)

    lineMap.values.count(_ > 1)
  }

  def buildMap(ventLines: Seq[Line]): Map[Point, Int] = {
    ventLines.foldLeft(Map.empty[Point, Int].withDefault(_ => 0)) { (acc1, line) =>
      line.points.foldLeft(acc1) { (acc2, point) =>
        acc2.updated(point, acc2(point) + 1)
      }
    }
  }

  def step2: Int = step2(inputLines)
  def step2(input: List[String]): Int = {
    val lineMap = buildMap(parse(input))

    lineMap.values.count(_ > 1)
  }

  def main(args: Array[String]): Unit = {

    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
