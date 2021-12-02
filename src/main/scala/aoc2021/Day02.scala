package aoc2021

import shared._

case object Day02 extends AocTools(day = 2) {

  case class Position(h: Int = 0, d: Int = 0) {
    def go(in: Instruction): Position = in match {
      case Instruction("forward", c) => copy(h = h + c)
      case Instruction("down", c)    => copy(d = d + c)
      case Instruction("up", c)      => copy(d = d - c)
    }
  }
  case class Position2(h: Int = 0, d: Int = 0, aim: Int = 0) {
    def go(in: Instruction): Position2 = in match {
      case Instruction("forward", c) => copy(h = h + c, d = d + (aim * c))
      case Instruction("down", c)    => copy(aim = aim + c)
      case Instruction("up", c)      => copy(aim = aim - c)
    }
  }

  case class Instruction(direction: String, count: Int) {}

  object Instruction {
    def apply(string: String): Instruction = {
      val parts = string.split(" ").toList
      Instruction(parts.head, parts(1).toInt)
    }

  }

  def parse(input: Seq[String]): Seq[Instruction] = input.map(Instruction(_))

  def process(input: Seq[Instruction]): Position = input.foldLeft(Position())((pos, i) => pos.go(i))
  def process2(input: Seq[Instruction]): Position2 = input.foldLeft(Position2())((pos, i) => pos.go(i))

  def step1: Int = step1(inputLines)
  def step1(input: Seq[String]): Int = {
    val Position(h, d) = process(parse(input))
    h * d
  }

  def step2: Int = step2(inputLines)
  def step2(input: Seq[String]): Int = {
    val Position2(h, d, _) = process2(parse(input))
    h * d
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
