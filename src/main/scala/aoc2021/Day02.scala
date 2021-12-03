package aoc2021

import shared._

case object Day02 extends AocTools(day = 2) {

  trait Processor {
    def apply(instructions: Seq[Instruction]): Position
  }

  object SimpleProcessor extends Processor {
    override def apply(instructions: Seq[Instruction]): Position = instructions.foldLeft(Position(0, 0, 0)) {
      case (Position(h, d, a), Instruction("forward", count)) => Position(h + count, d, a)
      case (Position(h, d, a), Instruction("down", count))    => Position(h, d + count, a)
      case (Position(h, d, a), Instruction("up", count))      => Position(h, d - count, a)
    }
  }
  object AimProcessor extends Processor {
    override def apply(instructions: Seq[Instruction]): Position = instructions.foldLeft(Position(0, 0, 0)) {
      case (Position(h, d, a), Instruction("forward", count)) => Position(h + count, d + (a * count), a)
      case (Position(h, d, a), Instruction("down", count))    => Position(h, d, a + count)
      case (Position(h, d, a), Instruction("up", count))      => Position(h, d, a - count)
    }
  }

  case class Position(height: Int = 0, depth: Int = 0, aim: Int = 0)

  case class Instruction(direction: String, count: Int)
  object Instruction {
    def apply(string: String): Instruction = {
      val parts = string.split(" ").toList
      Instruction(parts.head, parts(1).toInt)
    }

  }

  def parse(input: Seq[String]): Seq[Instruction] = input.map(Instruction(_))

  def process(input: Seq[Instruction], processor: Processor): Int = {
    val Position(h, d, _) = processor(input)
    h * d
  }

  def step1: Int = step1(inputLines)
  def step1(input: Seq[String]): Int = process(parse(input), SimpleProcessor)

  def step2: Int = step2(inputLines)
  def step2(input: Seq[String]): Int = process(parse(input), AimProcessor)

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1) // 1714950
    println("Step 2: " + step2) // 1281977850
  }
}
