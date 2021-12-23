package aoc2021

import shared._

import java.lang.Math.{max, min}

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

  case object On extends State {
    override lazy val flip = Off
  }

  case object Off extends State {
    override lazy val flip = On
  }

  case class Point(x: Long, y: Long, z: Long)

  case class Cuboid(minX: Long, maxX: Long, minY: Long, maxY: Long, minZ: Long, maxZ: Long, state: State) {
    def negate: Cuboid = copy(state = state.flip)

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

    def trimTo(cuboid: Cuboid) =
      if (
        cuboid.minX > this.maxX || cuboid.maxX < this.minX ||
        cuboid.minY > this.maxY || cuboid.maxY < this.minY ||
        cuboid.minZ > this.maxZ || cuboid.maxZ < this.minZ
      ) None
      else {
        val minX = max(this.minX, cuboid.minX)
        val maxX = min(this.maxX, cuboid.maxX)
        val minY = max(this.minY, cuboid.minY)
        val maxY = min(this.maxY, cuboid.maxY)
        val minZ = max(this.minZ, cuboid.minZ)
        val maxZ = min(this.maxZ, cuboid.maxZ)

        Some(Cuboid(minX, maxX, minY, maxY, minZ, maxZ, state))
      }

    val volume = (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

    def intersection(that: Cuboid): Option[Cuboid] = {
      val minX = max(this.minX, that.minX)
      val maxX = min(this.maxX, that.maxX)
      val minY = max(this.minY, that.minY)
      val maxY = min(this.maxY, that.maxY)
      val minZ = max(this.minZ, that.minZ)
      val maxZ = min(this.maxZ, that.maxZ)

      if (minX <= maxX && minY <= maxY && minZ <= maxZ) Some(copy(minX, maxX, minY, maxY, minZ, maxZ)) else None
    }
  }

  case class Cube(grid: Map[Point, State] = Map.empty.withDefaultValue(Off)) {
    def apply(instruction: Instruction) = turnToState(instruction.cuboid, instruction.state)

    def turnToState(cuboid: Cuboid, state: State) = Cube(grid ++ cuboid.points.map(_ -> state).toMap)

    def subCube(cuboid: Cuboid) = Cube(grid.view.filterKeys(cuboid.contains).toMap)

    def count(state: State) = grid.values.count(_ == state)

  }

  case class Instruction(cuboid: Cuboid, state: State)

  def parse(input: Seq[String]): Seq[Instruction] = input.map { case s"$state x=$minX..$maxX,y=$minY..$maxY,z=$minZ..$maxZ" =>
    Instruction(Cuboid(minX.toLong, maxX.toLong, minY.toLong, maxY.toLong, minZ.toLong, maxZ.toLong, State.fromString(state)), State.fromString(state))
  }

  def step1: Long = step1(inputLines)

  def step1(input: List[String]): Long = {
    val target = Cuboid(-50, 50, -50, 50, -50, 50, Off)
    val instructions = parse(input)
    val instructionsClipped = instructions.zip(instructions.map(_.cuboid.trimTo(target))).collect { case (instruction, Some(cuboid)) =>
      instruction.copy(cuboid = cuboid)
    }

    val resultingCube = instructionsClipped.foldLeft(Cube())((cube, instruction) => cube(instruction))
    resultingCube.count(On)
  }

  def step2: Long = step2(inputLines)

  def step2(input: List[String]): Long = {
    val instructions = parse(input)

    val resultCubes = instructions.foldLeft(Seq.empty[Cuboid]) { (seq, ins) =>
      val intersections = seq.flatMap(_.intersection(ins.cuboid).map(_.negate))
      if (ins.state == On) seq ++ intersections :+ ins.cuboid else seq ++ intersections
    }

    val (on, off) = resultCubes.partition(_.state == On)
    on.map(_.volume).sum - off.map(_.volume).sum

  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1)
    println("Step 2: " + step2)
  }
}
