package aoc2021

import shared._

case object Day17 extends AocTools(day = 17) {

  case class Probe(xv: Int, yv: Int) {
    private lazy val xs = LazyList.iterate((0, xv)) { case (x, v) => (x + v, Math.max(v - 1, 0)) }.map(_._1)
    private lazy val ys = LazyList.iterate((0, yv)) { case (y, v) => (y + v, v - 1) }.map(_._1)

    def highestY: Int = ys.sliding(2).dropWhile(y => y.head <= y.last).next().head

    def hits(target: Area): Boolean = {
      val xList = xs.takeWhile(_ <= target.maxX)
      val yList = ys.takeWhile(_ >= target.minY)
      xList.zip(yList).exists { case (x, y) =>
        x >= target.minX && y <= target.maxY
      }
    }
  }

  case class Area(minX: Int, maxX: Int, minY: Int, maxY: Int) {

    private def probes: Seq[Probe] = for {
      vx <- 1 to (maxX + 1)
      vy <- minY to (-minY + 1)
    } yield Probe(vx, vy)

    private lazy val probesThatHit: Seq[Probe] = probes.filter(_.hits(this))

    def highestY: Int = probesThatHit.map(_.highestY).max

    def hits: Int = probesThatHit.size
  }

  def parse(input: String): Area = input match {
    case s"target area: x=$minX..$maxX, y=$minY..$maxY" => Area(minX.toInt, maxX.toInt, minY.toInt, maxY.toInt)
  }

  def step1: Int = step1(inputLines.head)

  def step1(input: String): Int = parse(input).highestY

  private def step2: Int = step2(inputLines.head)

  def step2(input: String): Int = parse(input).hits

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 10878
    println("Step 2: " + step2) // 4716
  }

}
