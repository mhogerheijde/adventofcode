package net.hogerheijde.aoc2015.day7

import scala.+:

import net.hogerheijde.aoc2015.day7.Gate.Unary.Jumper
import net.hogerheijde.aoc2015.day7.Value.Literal
import net.hogerheijde.aoc2015.day7.Value.Wire
import net.hogerheijde.aoc2015.util.Day
//object Expressions {
//  def ~(value: Value.Literal) = Value.Literal(~value.value)
//}

sealed trait Value
object Value {
  case class Literal private(value: Int) extends Value {
    def unary_~() = Literal.create(value ^ 0xFFFF)
    def >>(amount: Int) = Literal.create(value >> amount)
    def <<(amount: Int) = Literal.create(value << amount)
    def &(other: Literal) = Literal.create(value & other.value)
    def |(other: Literal) = Literal.create(value | other.value)
  }
  object Literal {
    def create(value: Int): Literal = Literal(value & 0x0000FFFF)
    def unapply(input: String): Option[Literal] = input match {
      case value if value.forall(_.isDigit) => Some(Literal.create(Integer.parseInt(value)))
      case _ => None
    }
  }
  case class Wire(name: String) extends Value with Ordered[Wire] {
    override def compare(that: Wire): Int = name.compare(that.name)
  }
  object Wire {
    def unapply(input: String): Option[Wire] = input match {
      case wire if wire.forall(_.isLower) => Some(Wire(wire))
      case _ => None
    }
  }

  def unapply(input: String): Option[Value] = input match {
    case Literal(l) => Some(l)
    case Wire(w) => Some(w)
    case _ => None
  }
}



sealed trait Gate {
  def resolve(signalFinder: Value => Value.Literal): Value.Literal
  def inputs: Set[Wire]
}

object Gate {
  def unapply(s: String): Option[Gate] = {
    s match {
      case Unary(n) => Some(n)
      case Binary(n) => Some(n)
      case _ => None
    }
  }

  sealed trait Unary extends Gate {
    def input: Value
    val inputs = Set(input).collect { case w: Wire => w }
  }
  object Unary {
    case class Jumper(input: Value) extends Unary {
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = signalFinder(input)
    }

    case class Not(input: Value) extends Unary {
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = ~signalFinder(input)
    }
    case class LeftShift(amount: Int, input: Value) extends Unary {
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = signalFinder(input) << amount
    }
    case class RightShift(amount: Int, input: Value) extends Unary{
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = signalFinder(input) >> amount
    }

    def unapply(input: String): Option[Unary] = input match {
      case s"${Value(v)}" => Some(Jumper(v))
      case s"NOT ${Value(w)}" => Some(Not(w))
      case s"${Value(w)} LSHIFT $amount" if amount.forall(_.isDigit) => Some(LeftShift(Integer.parseInt(amount), w))
      case s"${Value(w)} RSHIFT $amount" if amount.forall(_.isDigit) => Some(RightShift(Integer.parseInt(amount), w))
      case _ => None
    }
  }

  sealed trait Binary extends Gate {
    def left: Value
    def right: Value
    val inputs = Set(left, right).collect { case w: Wire => w } // left biased
  }
  object Binary {
    case class And(left: Value, right: Value) extends Binary {
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = signalFinder(left) & signalFinder(right)
    }
    case class Or(left: Value, right: Value) extends Binary {
      override def resolve(signalFinder: Value => Value.Literal): Value.Literal = signalFinder(left) | signalFinder(right)
    }

    def unapply(input: String): Option[Binary] = input match {
      case s"${Value(left)} AND ${Value(right)}" => Some(And(left, right))
      case s"${Value(left)} OR ${Value(right)}" => Some(Or(left, right))
      case _ => None
    }
  }

}

object Day7 extends Day[Int, Int] {

  type Model = Map[Value, Gate]
  /**
    * The name of the day
    *
    * @return
    */
  override def name: String = "Day 7"

  /**
   * Parses the string of the puzzle input to the model type
   *
   * @return
   */
    override def parse: String => Model = s => {
      s.linesIterator
        .map {
          case s"${Gate(g)} -> ${Value.Wire(w)}" => (w, g)
          case line => throw new Exception(s"Could not parse $line")
        }
        .toMap
    }

//  override def parse: String => Map[Value, Gate] = _ => {
//    """123 -> x
//      |456 -> y
//      |x AND y -> d
//      |x OR y -> e
//      |x LSHIFT 2 -> f
//      |y RSHIFT 2 -> g
//      |NOT x -> h
//      |NOT y -> i"""
//      .stripMargin
//      .linesIterator
//      .map {
//        case s"${Gate(g)} -> ${Value.Wire(w)}" => (w, g)
//        case line => throw new Exception(s"Could not parse $line")
//      }
//      .toMap
//  }

  override def part1(gatesByOutput: Model): Int = {
    def signalFinder(wires: Map[Value.Wire, Value.Literal])(signal: Value): Value.Literal = signal match {
      case l: Value.Literal => l
      case w: Value.Wire => wires(w)
    }

    val wirelist = Graph.topologicalSort(gatesByOutput).foldRight(Map.empty[Value.Wire, Value.Literal]) {
      case (nextWire: Wire, acc) =>
        val gate = gatesByOutput(nextWire)
        acc.updated(nextWire, gate.resolve(signalFinder(acc)(_)))
      case (_, acc) => acc
    }

    wirelist.keys.toSeq.sorted.foreach { wire =>
      println(s"$wire => ${wirelist(wire)}")
    }
//    println(wirelist.map { case (k, v) => s"$k => $v" }.mkString("\n") )

    wirelist(Wire("a")).value
  }
  override def part2(input:  Model): Int = {
    val a1 = part1(input)
    val newInput = input.updated(Wire("b"), Jumper(Literal(a1)))
    part1(newInput)
  }
}



object Graph {
  private case class SortState(discovered: Set[Value] = Set(), activeNodes: Set[Value] = Set(), tsOrder: List[Value] = List())

  def topologicalSort(input: Map[Value, Gate]): List[Value] = {
    def sortPartial(currentState: SortState, nextWire: Value): SortState = {
      assert(!input(nextWire).inputs.exists(currentState.activeNodes.contains(_)), "Cannot handle cyclic graphs")

      val tempState = currentState.copy(
        discovered = currentState.discovered + nextWire,
        activeNodes = currentState.activeNodes + nextWire,
      )

      val nextState = input(nextWire).inputs
          .filterNot(tempState.discovered)
          .foldLeft(tempState)(sortPartial(_, _))

      nextState.copy(
        tsOrder = nextWire :: nextState.tsOrder,
        activeNodes = nextState.activeNodes - nextWire,
      )
    }

    val stateAfterSearch = input.keys.foldLeft(SortState()) { (currentState, nextWire) =>
      if (currentState.discovered(nextWire)) { currentState }
      else { sortPartial(currentState, nextWire) }
    }
    stateAfterSearch.tsOrder
  }
}