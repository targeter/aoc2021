package aoc2021

import shared._

import scala.util.Try

case object Day13 extends AocTools(day = 13) {

  val DOT = '█'
  val BLANK = ' '

  type Grid = Seq[Seq[Char]]

  sealed trait Instruction {
    def fold(grid: Grid): Grid
  }

  def printGrid(grid: Grid): Unit = {
    grid.foreach(l => println(l.mkString))
  }

  def merge(a: Grid, b:Grid): Grid = {
    val yOff = (a.length - b.length)
    val xOff = (a.head.length - b.head.length)
    a.indices.map{ y=>
      a.head.indices.map { x =>
          Try(if(a(y)(x) == DOT || b(y - yOff)(x - xOff) == DOT) DOT else BLANK).getOrElse(BLANK)
      }
    }
  }

  case class FoldX(position: Int) extends Instruction {
    override def fold(grid: Grid): Grid = {
      val (top, bottom) =grid.transpose.splitAt(position)
      merge(top.transpose, bottom.tail.transpose.map(_.reverse))
    }
  }
  case class FoldY(position: Int) extends Instruction {
    override def fold(grid: Grid): Grid = {
      val (top, bottom) =grid.splitAt(position)
      merge(top, bottom.tail.reverse)
    }
  }

  case class Point(y: Int, x: Int)

  def parse(input: Seq[String]) = {
    val blankIndex = input.indexOf("")
    val points = input.slice(0, blankIndex).map {
      case s"$x,$y" => (x.toInt, y.toInt)
    }
    val instructions = input.slice(blankIndex + 1, input.length).map {
      case s"fold along y=$pos" => FoldY(pos.toInt)
      case s"fold along x=$pos" => FoldX(pos.toInt)
    }
    val (xs, ys) = points.unzip

    val grid = (0 to ys.max).map { y =>
      (0 to xs.max).map { x =>
        if(points.contains((x,y))) DOT else BLANK
      }
    }

    (grid, instructions)
  }

  def step1: Int = step1(inputLines)

  def step1(input: Seq[String]): Int = {
    val (grid, instructions) = parse(input)
    instructions.head.fold(grid).flatten.count(_ == DOT)
  }

  def step2: Int = step2(inputLines)

  def step2(input: Seq[String]): Int = {
    val (grid: Grid, instructions: Seq[Instruction]) = parse(input)
    val result: Grid = instructions.foldLeft[Grid](grid){(grid: Grid, i: Instruction) => i.fold(grid)}
    printGrid(result)
    0
  }

  def main(args: Array[String]): Unit = {
//    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }

}
