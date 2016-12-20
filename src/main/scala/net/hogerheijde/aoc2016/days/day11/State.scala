package net.hogerheijde.aoc2016.days.day11

import scala.collection.immutable.Set

case class Processor(tree: Node)

object Foo {



}

case class Node(children: IndexedSeq[Node], state: State, move: Option[Move] = None)
object Node {
  def fromState(state: State): Node = {
    generate(Node(IndexedSeq(), state))
  }
  def generate(node: Node): Node = {
    node.state match {
      case InvalidState => node
      case SolvedState(_) => node
      case ValidState(elevator, items) =>

        val sortedItems = items.toIndexedSeq.sortBy(_.name) // for consistent ordering of the solution
      val possibleCombinations = (1 to 2).flatMap(sortedItems.combinations)

        val childNodes = for {
          nextFloor <- IndexedSeq(elevator.floor.down, elevator.floor.up).flatten
          carry <- possibleCombinations
        } yield {

          val newItems = (items -- carry) ++ (carry map {
            case g: Generator => g.copy(floor = nextFloor)
            case m: Microchip => m.copy(floor = nextFloor)
          } )

          val move = Move(nextFloor, carry)
          val generatedState = State(elevator.copy(floor = nextFloor), items = newItems)
          val possibleNode = Node(IndexedSeq(), generatedState, Some(move))

          generate(possibleNode)
        }

        node.copy(children = childNodes)
    }

  }
}

case class Move(floor: Floor, items: IndexedSeq[Item])

trait State
case object InvalidState extends State
case class SolvedState(items: Set[Item]) extends State {

  lazy val itemsByName = items.map(i => (i.name, i)).toMap
  lazy val itemNamesSorted = itemsByName.keys.toIndexedSeq.sorted
  lazy val itemsByFloor = items.groupBy(i => i.floor).withDefaultValue(Set())

  override def toString: String = {

    def floorInventory(floor: Floor): String = {
      val q = itemNamesSorted flatMap { name =>
        itemsByName.get(name) map { item =>
          itemsByFloor(floor).find(_ == item).map(_.name).getOrElse(". ")
        }
      }
      q.mkString(" ")
    }

    s"${F4.display} E   ${floorInventory(F4)}\n" +
      s"${F3.display} . | ${floorInventory(F3)}\n"+
      s"${F2.display} . | ${floorInventory(F2)}\n"+
      s"${F1.display} . | ${floorInventory(F1)}\n"
  }
}
case class ValidState(elevator: Elevator, items: Set[Item]) extends State {

  lazy val itemsByName = items.map(i => (i.name, i)).toMap
  lazy val itemNamesSorted = itemsByName.keys.toIndexedSeq.sorted
  lazy val itemsByFloor = items.groupBy(i => i.floor).withDefaultValue(Set())



  override def toString: String = {

    def floorInventory(floor: Floor): String = {
      val q = itemNamesSorted flatMap { name =>
        itemsByName.get(name) map { item =>
          itemsByFloor(floor).find(_ == item).map(_.name).getOrElse(". ")
        }
      }
      val e = if (elevator.floor == floor) { "E   " } else { ". | " }
      e + q.mkString(" ")
    }

    s"${F4.display} ${floorInventory(F4)}\n" +
      s"${F3.display} ${floorInventory(F3)}\n"+
      s"${F2.display} ${floorInventory(F2)}\n"+
      s"${F1.display} ${floorInventory(F1)}\n"
  }

}
object State {
  def apply(elevator: Elevator, items: Set[Item]): State = {
    if (isSolved(elevator, items)) {
      SolvedState(items)
    } else if (isConsistent(items)) {
      ValidState(elevator, items)
    } else {
      InvalidState
    }
  }

  def isSolved(elevator: Elevator, items: Set[Item]): Boolean = elevator.floor == F4 && items.forall(_.floor == F4)

  def isConsistent(items: Set[Item]): Boolean = {
    items.groupBy(_.floor) forall { case (floor, items) =>
      val (generators, chips) = items partition {
        case g: Generator => true
        case m: Microchip => false
      }

      val mustHaveGenerators = chips.map(_.isotope)
      if (generators.isEmpty) {
        true
      } else {
        mustHaveGenerators forall (isotope => generators.exists(g => g.isotope == isotope))
      }
    }
  }
}



case class Elevator(floor: Floor) {
  val display = "E"
}

trait Item {
  def isotope: Char
  def name: String
  def floor: Floor
}
case class Generator(isotope: Char, floor: Floor) extends Item {
  val name = isotope + "G"
}
case class Microchip(isotope: Char, floor: Floor) extends Item {
  val name = isotope + "M"
}

trait Floor {
  def down: Option[Floor]
  def up: Option[Floor]
}

case object F1 extends  Floor { val display = "F1"; val down = None;     val up = Some(F2) }
case object F2 extends  Floor { val display = "F2"; val down = Some(F1); val up = Some(F3) }
case object F3 extends  Floor { val display = "F3"; val down = Some(F2); val up = Some(F4) }
case object F4 extends  Floor { val display = "F4"; val down = Some(F3); val up = None }