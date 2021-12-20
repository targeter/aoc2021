package aoc2021

import shared._
import shared.ops.StringOps

import java.util.Comparator

case object Day20 extends AocTools(day = 20) {

  val Light = '#'
  val Dark = '.'

  case class Point(y: Int, x: Int)

  object Point {
    implicit val ordering: Ordering[Point] = Ordering.by[Point, (Int, Int)](p => p.y -> p.x)(Ordering.Tuple2[Int, Int])
  }

  case class Dimensions(minX: Int, maxX: Int, minY: Int, maxY: Int) {
    def points: Seq[Point] = for {
      y <- minY to maxY
      x <- minX to maxX
    } yield Point(y, x)

    def grow(by: Int): Dimensions = copy(minX - by, maxX + by, minY - by, maxY + by)
  }

  case class Image(grid: Map[Point, Char], enhancements: Seq[Char], default: Char = Dark) {
    def dimensions: Dimensions = Dimensions(
      grid.keySet.minBy(_.x).x,
      grid.keySet.maxBy(_.x).x,
      grid.keySet.minBy(_.y).y,
      grid.keySet.maxBy(_.y).y
    )

    def value(point: Point): Char = grid.getOrElse(point, default)

    def samplePoints(point: Point): Seq[Point] = for {
      yDiff <- -1 to 1
      xDiff <- -1 to 1
    } yield Point(point.y - yDiff, point.x - xDiff)

    def sampleValues(point: Point): Seq[Char] = {
      samplePoints(point).sorted.map(value)
    }

    def sampleNumber(point: Point) = sampleValues(point)
      .map {
        case `Light` => '1'
        case `Dark`  => '0'
      }
      .mkString
      .toBinInt

    def enhancePixel(point: Point): Char = enhancements(sampleNumber(point))

    def enhance: Image = {
      val newDefault = if (default == Dark) Light else Dark
      val enhancedGrid = dimensions.grow(1).points.map(p => p -> enhancePixel(p)).toMap
      copy(enhancedGrid, default = newDefault)
    }

  }

  def parse(input: Seq[String]) = {
    val program = input.head.toList
    val grid = input
      .drop(2)
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.map { case (c, x) =>
          Point(y, x) -> c
        }
      }
      .toMap
    Image(grid, program)
  }

  def step1: Int = step1(inputLines)

  def step1(input: List[String]): Int = {
    val original = parse(input)
    val enhanced = original.enhance.enhance
    enhanced.grid.values.count(_ == Light)
  }

  def step2: Int = step2(inputLines)

  def step2(input: List[String]): Int = {
    val original = parse(input)
    val enhanced = LazyList.iterate(original)(_.enhance)(50)
    enhanced.grid.values.count(_ == Light)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
