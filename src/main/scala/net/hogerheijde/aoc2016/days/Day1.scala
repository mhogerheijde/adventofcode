package net.hogerheijde.aoc2016.days

import net.hogerheijde.aoc2016.days.Day1.Distance
import net.hogerheijde.aoc2016.model.Coordinates
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

  private[days] def processInstructions(): Coordinates = {
    val startingCoordinates: Coordinates = Coordinates(0,0)
    val startingDirection: Direction = North

    val (coordinates, _) = instructions.foldLeft( (startingCoordinates, startingDirection) ) { case ((currentCoordinates, currentDirection), nextInstruction) =>
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
      val newCoordinates = currentCoordinates.update(newDirection, nextInstruction.amount)

      (newCoordinates, newDirection)
    }

    coordinates
  }

  def run(): Distance = {
    val coordinates = processInstructions()
    coordinates.distance
  }
}

object Day1 {

  def build(instruction: String): Day1 = new Day1(parse(instruction))

  type Distance = Int


  private[days] def parse(input: String): IndexedSeq[Instruction] = {
    val splittedDirections = input.split(",")
    splittedDirections.flatMap(parseDirection).toIndexedSeq
  }

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
