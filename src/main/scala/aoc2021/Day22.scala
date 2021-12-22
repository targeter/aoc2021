package aoc2021

import shared._

case object Day22 extends AocTools(day = 22) {

  sealed trait State {
    def flip: State
  }

  object State {
    def fromString(state: String) = state match {
      case "on"  => On
      case "off" => Off
    }
  }

  object On extends State {
    override lazy val flip = Off
  }

  object Off extends State {
    override lazy val flip = On
  }

  case class Point(x: Int, y: Int, z: Int)

  case class Cuboid(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int) {
    lazy val points = for {
      x <- minX to maxX
      y <- minY to maxY
      z <- minZ to maxZ
    } yield Point(x, y, z)

    def contains(point: Point): Boolean = {
      minX <= point.x && point.x <= maxX &&
      minY <= point.y && point.y <= maxY &&
      minZ <= point.z && point.z <= maxZ
    }

    def trimTo(cuboid: Cuboid) = {
      if (
        cuboid.minX > this.maxX || cuboid.maxX < this.minX ||
        cuboid.minY > this.maxY || cuboid.maxY < this.minY ||
        cuboid.minZ > this.maxZ || cuboid.maxZ < this.minZ
      ) None
      else {
        val minX = Math.max(this.minX, cuboid.minX)
        val maxX = Math.min(this.maxX, cuboid.maxX)
        val minY = Math.max(this.minY, cuboid.minY)
        val maxY = Math.min(this.maxY, cuboid.maxY)
        val minZ = Math.max(this.minZ, cuboid.minZ)
        val maxZ = Math.min(this.maxZ, cuboid.maxZ)

        Some(Cuboid(minX, maxX, minY, maxY, minZ, maxZ))
      }
    }
  }

  case class Cube(grid: Map[Point, State] = Map.empty.withDefaultValue(Off)) {
    def apply(instruction: Instruction) = turnTo(instruction.cuboid, instruction.state)

    def turnTo(cuboid: Cuboid, state: State) = Cube(grid ++ cuboid.points.map(_ -> state).toMap)

    def subCube(cuboid: Cuboid) = Cube(grid.view.filterKeys(cuboid.contains).toMap)

    def count(state: State) = grid.values.count(_ == state)

  }

  case class Instruction(cuboid: Cuboid, state: State)

  def parse(input: Seq[String]): Seq[Instruction] = input.map { case s"$state x=$minX..$maxX,y=$minY..$maxY,z=$minZ..$maxZ" =>
    Instruction(Cuboid(minX.toInt, maxX.toInt, minY.toInt, maxY.toInt, minZ.toInt, maxZ.toInt), State.fromString(state))
  }

  def step1: Int = step1(inputLines)

  def step1(input: List[String]): Int = {
    val target = Cuboid(-50, 50, -50, 50, -50, 50)
    val instructions = parse(input)
    val instructionsClipped = instructions.zip(instructions.map(_.cuboid.trimTo(target))).collect {
      case (instruction, Some(cuboid)) => instruction.copy(cuboid = cuboid)
    }

    val resultingCube = instructionsClipped.foldLeft(Cube())((cube, instruction) => cube(instruction))
    resultingCube.count(On)
  }

  def step2: Int = step2(inputLines)

  def step2(input: List[String]): Int = {
    val target = Cuboid(-50, 50, -50, 50, -50, 50)
    val instructions = parse(input)

    val resultingCube = instructions.foldLeft(Cube())((cube, instruction) => cube(instruction))
    resultingCube.count(On)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
