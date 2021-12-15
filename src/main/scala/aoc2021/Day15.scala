package aoc2021

import shared._

import scala.collection.mutable

case object Day15 extends AocTools(day = 15) {

  case class Point(y: Int, x: Int)

  case class CaveMap(grid: Seq[Seq[Int]]) {
    val positions: Seq[Point] = for {
      y <- grid.indices
      x <- grid.head.indices
    } yield Point(y, x)

    def value(p: Point): Int = grid(p.y)(p.x)

    def adjacent(p: Point): Seq[Point] = Seq(
      (p.y - 1, p.x),
      (p.y + 1, p.x),
      (p.y, p.x - 1),
      (p.y, p.x + 1)
    ).collect { case (y, x) if grid.indices.contains(y) && grid.head.indices.contains(x) => Point(y, x) }

    def exit: Point = Point(grid.indices.last, grid.head.indices.last)

    def mapValues(f: Int => Int): CaveMap = CaveMap(grid.map(_.map(f)))

    def nextIteration: CaveMap = mapValues(i => if (i + 1 <= 9) i + 1 else i - 8)

    def appendHorizontal(that: CaveMap): CaveMap = {
      if (this.grid.isEmpty) that
      else CaveMap(this.grid.zip(that.grid).map(tup => tup._1 ++ tup._2))
    }

    def appendVertical(that: CaveMap): CaveMap = CaveMap(this.grid ++ that.grid)

    def expand(nrOfIterations: Int): CaveMap = {
      val cellList: LazyList[CaveMap] = LazyList.iterate(this)(_.nextIteration)
      def concat(cells: Seq[CaveMap]): CaveMap = cells.fold(CaveMap(List.empty))((a, b) => a.appendHorizontal(b))

      def row(i: Int): CaveMap = concat(cellList.slice(i, i + nrOfIterations))

      val rows = Seq.tabulate(5)(row)
      rows.fold(CaveMap(List.empty))((a, b) => a.appendVertical(b))
    }

    def print(): Unit = grid.map(_.mkString).foreach(println)

  }

  val Entrance: Point = Point(0, 0)

  def parse(lines: Seq[String]): CaveMap = CaveMap(lines.map(_.map(_.toString.toInt)))

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = {
    val caveMap = parse(input)
    findDistance(caveMap, Entrance, caveMap.exit).getOrElse(sys.error("No route"))
  }

  def step2: Int = step2(inputLines)

  def step2(input: Seq[String]): Int = {
    val caveMap = parse(input).expand(5)
    findDistance(caveMap, Entrance, caveMap.exit)
  }.getOrElse(sys.error("No route"))

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 423
    println("Step 2: " + step2) // 2778
  }

  def findDistance(caveMap: CaveMap, start: Point, goal: Point): Option[Int] = {
    implicit val ordering: Ordering[(Point, Int)] = Ordering.by[(Point, Int), Int](_._2).reverse

    val knownDistances = mutable.Map.empty[Point, Int]
    val toSearch = mutable.PriorityQueue(start -> 0)

    while (toSearch.nonEmpty) {
      val (currentNode, distanceToCurrentNode) = toSearch.dequeue()
      if (currentNode == goal) return Some(distanceToCurrentNode)
      if (!knownDistances.contains(currentNode)) {
        knownDistances(currentNode) = distanceToCurrentNode

        caveMap
          .adjacent(currentNode)
          .filterNot(knownDistances.contains)
          .map(p => p -> (caveMap.value(p) + distanceToCurrentNode))
          .foreach(toSearch.enqueue(_))
      }
    }

    None // No path to goal
  }

}
