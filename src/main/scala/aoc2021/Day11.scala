package aoc2021

import shared._

case object Day11 extends AocTools(day = 11) {

  case class Point(y: Int, x: Int)

  case class Octopus(energy: Int, flashCount: Long = 0, hasFlashed: Boolean = false) {
    val shouldFlash: Boolean = !hasFlashed && energy > 9
    def increase: Octopus = copy(energy = energy + 1)

    def flash: Octopus = if (shouldFlash) copy(flashCount = flashCount + 1, hasFlashed = true) else this
    def reset: Octopus = if (hasFlashed) copy(energy = 0, hasFlashed = false) else this
  }

  case class CaveMap(grid: Map[Point, Octopus]) {

    def adjacent(p: Point): Seq[Point] = List(
      Point(p.y - 1, p.x - 1),
      Point(p.y - 1, p.x),
      Point(p.y - 1, p.x + 1),
      Point(p.y, p.x - 1),
      Point(p.y, p.x + 1),
      Point(p.y + 1, p.x - 1),
      Point(p.y + 1, p.x),
      Point(p.y + 1, p.x + 1)
    ).collect {
      case p if grid.contains(p) => p
    }

    def increase(points: Iterable[Point]): CaveMap = copy(points.foldLeft(grid)((grid, p) => grid.updated(p, grid(p).increase)))

    def flash(points: Iterable[Point]): CaveMap = copy(points.foldLeft(grid) { (grid, p) => grid.updatedWith(p)(_.map(_.flash)) })

    def resetEnergy: CaveMap = copy(grid.view.mapValues(_.reset).toMap)

    def nextDay: CaveMap = increase(grid.keys).flash

    def flash: CaveMap = {
      val aboutToFlash = grid.collect { case (p, o) if o.shouldFlash => p }
      if (aboutToFlash.isEmpty) this.resetEnergy
      else {
        val adjacentToFlash = aboutToFlash.flatMap(adjacent)

        increase(adjacentToFlash).flash(aboutToFlash).flash
      }
    }

    def totalFlashCount: Long = grid.values.map(_.flashCount).sum

  }

  object CaveMap {

    def parseIn(in: Seq[String]): CaveMap = parse(in.map(_.map(_.toString.toInt)))

    def parse(grid: Seq[Seq[Int]]): CaveMap = {
      val positions: Seq[Point] = for {
        y <- grid.indices
        x <- grid.head.indices
      } yield Point(y, x)

      CaveMap(positions.map(point => point -> Octopus(grid(point.y)(point.x))).toMap)
    }

  }

  def step1: Long = step1(inputLines)

  def step1(input: Seq[String]): Long = {
    val initialMap = CaveMap.parseIn(input)
    val days = LazyList.iterate(initialMap)(_.nextDay)

    days(100).totalFlashCount

  }

  def step2: Long = step2(inputLines)

  def step2(input: Seq[String]): Long = {
    val initialMap = CaveMap.parseIn(input)
    val day = LazyList.iterate(initialMap)(_.nextDay).zipWithIndex.find(_._1.grid.forall(_._2.energy == 0))
    day.map(_._2).get
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
