package aoc2021

import shared._

import scala.annotation.tailrec

case object Day09 extends AocTools(day = 9) {

  case class Point(y: Int, x: Int)

  case class CaveMap(grid: Seq[Seq[Int]]) {
    val positions: Seq[Point] = for {
      y <- grid.indices
      x <- grid.head.indices
    } yield Point(y, x)

    def value(p: Point): Int = grid(p.y)(p.x)

    def adjacent(p: Point): Seq[Point] = List(
      (p.y - 1, p.x),
      (p.y + 1, p.x),
      (p.y, p.x - 1),
      (p.y, p.x + 1)
    ).collect { case (y, x) if grid.indices.contains(y) && grid.head.indices.contains(x) => Point(y, x) }

    def adjacentValues(p: Point): Seq[Int] = adjacent(p).map { case Point(y, x) => grid(y)(x) }

    def isLow(p: Point): Boolean = adjacentValues(p).forall(_ > value(p))
    def risk(p: Point): Int = value(p) + 1

    def lowPoints: Seq[Point] = positions.filter(isLow)
    def lowPointsRisk: Seq[Int] = lowPoints.map(risk)

    def findBasin(lp: Point): List[Point] = {
      @tailrec
      def findBasin(adjacentToGo: List[Point], basinSoFar: List[Point]): List[Point] = adjacentToGo match {
        case Nil => basinSoFar
        case pos :: tail =>
          val todo = adjacent(pos).filter(p => value(p) != 9 && !basinSoFar.contains(p))
          val newBasin = if (basinSoFar.contains(pos)) basinSoFar else basinSoFar :+ pos
          findBasin(tail ++ todo, newBasin)
      }

      findBasin(List(lp), List.empty[Point])
    }

  }

  def parse(lines: Seq[String]): CaveMap = {
    val grid: Seq[Seq[Int]] = lines.map(_.map(_.toString.toInt))
    CaveMap(grid)

  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = parse(input).lowPointsRisk.sum

  def step2: Int = step2(inputLines)

  def step2(input: Seq[String]): Int = {
    val cm = parse(input)
    cm.lowPoints.map(cm.findBasin).map(_.size).sorted.takeRight(3).product
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
