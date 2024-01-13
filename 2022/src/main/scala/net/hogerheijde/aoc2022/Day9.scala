package net.hogerheijde.aoc2022

import scala.annotation.tailrec
import scala.annotation.targetName

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.CoordinateTranslation
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc2022.Day9.Direction

object Day9 extends Day[Int, Int]:
  override type Model = Seq[Direction]

  enum Direction(amount: Int):
    def step: Option[Direction] =
      this match {
        case Up(a) if a > 1 => Some(Up(a - 1))
        case Down(a) if a > 1 => Some(Down(a - 1))
        case Left(a) if a > 1 => Some(Left(a - 1))
        case Right(a) if a > 1 => Some(Right(a - 1))
        case _ => None
      }

    case Up(amount: Int) extends Direction(amount)
    case Down(amount: Int) extends Direction(amount)
    case Left(amount: Int) extends Direction(amount)
    case Right(amount: Int) extends Direction(amount)


  override def parse(input: String): Day9.Model = input.trim.linesIterator.map { line =>
    line match {
      case s"U ${IsInteger(a)}" => Direction.Up(a)
      case s"D ${IsInteger(a)}" => Direction.Down(a)
      case s"L ${IsInteger(a)}" => Direction.Left(a)
      case s"R ${IsInteger(a)}" => Direction.Right(a)
    }
  }.toSeq

  case class State(head: Coordinate, tail: Coordinate, visited: Seq[Coordinate]):
    def step(direction: Direction): State =
      burstSteps(direction)._1

    @tailrec
    private def burstSteps(direction: Direction): (State, Option[Direction]) =
      val (newHead, leftOverDirection) = head.move(direction)
      val newTail = tail.moveTowards(newHead)

      val newState = State(newHead, newTail, visited :+ newTail)
      leftOverDirection match {
        case None => (newState, leftOverDirection)
        case Some(d) => newState.burstSteps(d)
      }

  object State:
    def apply(): State = State(Coordinate(0, 0), Coordinate(0, 0), Seq(Coordinate(0, 0)))




  override def part1(input: Day9.Model): Int =
    val state = input.foldLeft(State()) { case (acc, nextDir) => acc.step(nextDir)}
    state.visited.toSet.size

  override def part2(input: Day9.Model): Int = ???

object Vector:
  val leftUp: (Int, Int) = (-1, -1)
  val up: (Int, Int) = (-1, 0)
  val rightUp: (Int, Int) = (-1, 1)
  val left: (Int, Int) = (0, -1)
  val center: (Int, Int) = (0, 0)
  val right: (Int, Int) = (0, 1)
  val leftDown: (Int, Int) = (1,-1)
  val down: (Int, Int) = (1,0)
  val rightDown: (Int, Int) = (1,1)

extension (c: Coordinate)
  def leftUp = c.transpose.leftUp
  def up = c.transpose.up
  def rightUp = c.transpose.rightUp
  def left = c.transpose.left
  def right = c.transpose.right
  def leftDown = c.transpose.leftDown
  def down = c.transpose.down
  def rightDown = c.transpose.rightDown




  def moveTowards(other: Coordinate): Coordinate =
    if (c.distance(other) <= Math.sqrt(2))
      c
    else
      val translation = c diff other match {
        case ( 2, -2) => Vector.leftDown
        case ( 2, -1) => Vector.leftDown
        case ( 2,  0) => Vector.down
        case ( 2,  2) => Vector.rightDown
        case ( 2,  1) => Vector.rightDown

        case ( 1, -2) => Vector.leftDown
        case ( 1,  2) => Vector.rightDown

        case ( 0,  -2) => Vector.left
        case ( 0,  2) => Vector.right

        case (-1, -2) => Vector.leftUp
        case (-1,  2) => Vector.rightUp

        case (-2, -2) => Vector.leftUp
        case (-2, -1) => Vector.leftUp
        case (-2,  0) => Vector.up
        case (-2,  1) => Vector.rightUp
        case (-2,  2) => Vector.rightUp
      }
      c.apply(vector = translation)



  def move(d: Direction): (Coordinate, Option[Direction]) =
    val x1 = d match {
      case Direction.Up(a) => c.up
      case Direction.Down(a) => c.down
      case Direction.Left(a) => c.left
      case Direction.Right(a) => c.right
    }
    (x1, d.step)