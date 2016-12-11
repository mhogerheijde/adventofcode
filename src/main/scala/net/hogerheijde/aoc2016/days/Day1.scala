package net.hogerheijde.aoc2016.days

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.Day1.Distance
import net.hogerheijde.aoc2016.model.Coordinate
import net.hogerheijde.aoc2016.model.Direction
import net.hogerheijde.aoc2016.model.Instruction
import net.hogerheijde.aoc2016.model.GoLeft
import net.hogerheijde.aoc2016.model.GoRight
import net.hogerheijde.aoc2016.model.North
import net.hogerheijde.aoc2016.model.South
import net.hogerheijde.aoc2016.model.East
import net.hogerheijde.aoc2016.model.West

import scala.collection.immutable.IndexedSeq
import scala.util.Try



class Day1(instructions: IndexedSeq[Instruction]) {

  def run(): Distance = {
    val coordinates = Day1.processInstructions(instructions).last
    coordinates.distance
  }

  def runPart2(): Distance = {
    val coordinates = Day1.processInstructions(instructions)
    Util.getFirstDupe(coordinates) match {
      case Some(coordinate) => coordinate.distance
      case _ => throw new RuntimeException("No coordinate found!")
    }
  }
}

object Day1 {

  def build(instruction: String): Day1 = new Day1(parse(instruction))

  type Distance = Int

  /**
    * Expands a given set of instructions into a list of all coordinates that you travel past.
    */
  private[day] def processInstructions(instructions: IndexedSeq[Instruction]): IndexedSeq[Coordinate] = {
    val startingCoordinates = IndexedSeq(Coordinate(0,0))
    val startingDirection: Direction = North

    val (coordinates, _) = instructions.foldLeft( (startingCoordinates, startingDirection) ) { case ((accCoordinates, currentDirection), nextInstruction) =>
      val newDirection = (currentDirection, nextInstruction) match {
        case (North, GoRight(_)) => East
        case (North, GoLeft(_))  => West
        case (South, GoRight(_)) => West
        case (South, GoLeft(_))  => East
        case (East, GoRight(_))  => South
        case (East, GoLeft(_))   => North
        case (West, GoRight(_))  => North
        case (West, GoLeft(_))   => South
      }

      val previousCoordinate = accCoordinates.last
      val newCoordinates = previousCoordinate.update(newDirection, nextInstruction.amount)

      val allCoordinatesInBetween = previousCoordinate.expand(newCoordinates)

      (accCoordinates ++ allCoordinatesInBetween, newDirection)
    }

    coordinates
  }

  // Parses a string in the form of "L5, R4" into a list of instructions to go Left or Right for the listed amount
  // of steps. This plainly ignores incorrect instructions; i.e. things that don't start with either L or R and aren't
  // followed by something parseable to int.
  private[day] def parse(input: String): IndexedSeq[Instruction] = {
    val splittedDirections = input.split(",")
    splittedDirections.flatMap(parseDirection).toIndexedSeq
  }

  // Parses a single left or right instruction with its amount of steps associated with it
  private[days] def parseDirection(input: String): Option[Instruction] = {
    val trimmedInput = input.trim
    trimmedInput.head.toUpper match {
      case 'L' => parseAsInteger(trimmedInput.drop(1)) map GoLeft
      case 'R' => parseAsInteger(trimmedInput.drop(1)) map GoRight
      case _ => None
    }
  }

  private[days] def parseAsInteger(input:String): Option[Int] = {
    Try(Integer.parseInt(input.trim)).toOption
  }

}
