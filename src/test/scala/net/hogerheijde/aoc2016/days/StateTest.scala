package net.hogerheijde.aoc2016.days

import net.hogerheijde.aoc2016.days.day11.Elevator
import net.hogerheijde.aoc2016.days.day11.State
import net.hogerheijde.aoc2016.days.day11.F1
import net.hogerheijde.aoc2016.days.day11.F2
import net.hogerheijde.aoc2016.days.day11.F3
import net.hogerheijde.aoc2016.days.day11.F4
import net.hogerheijde.aoc2016.days.day11.Generator
import net.hogerheijde.aoc2016.days.day11.Item
import net.hogerheijde.aoc2016.days.day11.Microchip
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StateTest extends FlatSpec with Matchers {

  "State" should "show conflicting state for floor with fried microchip" in {

    State.isConsistent(Set(
      Microchip('A', F1),
      Generator('B', F1)
    )) should be (false)

    State.isConsistent(Set(
      Microchip('A', F1),
      Microchip('C', F2),
      Generator('B', F1)
    )) should be (false)

  }

  it should "find non-confliciting state for floor with only microchips" in {

    State.isConsistent(Set(
      Microchip('A', F1),
      Generator('B', F2)
    )) should be (true)

    State.isConsistent(Set(
      Microchip('A', F1),
      Microchip('C', F1),
      Generator('B', F2)
    )) should be (true)

  }

  it should "find non-confliciting state for floors with microchips connected" in {

    State.isConsistent(Set(
      Microchip('A', F1),
      Generator('A', F1),
      Generator('B', F1)
    )) should be (true)

  }

  it should "see that state is solved" in {

    // SETUP
    val elevator = Elevator(F4)
    val items: Set[Item] = Set(
      Microchip('A', F4),
      Generator('A', F4),
      Microchip('B', F4),
      Generator('B', F4),
      Microchip('C', F4),
      Generator('C', F4)
    )

    // CALL + VERIFY
    State.isSolved(elevator, items) should be (true)

  }

  it should "see that state is not solved when elevator is not at F4" in {

    // SETUP
    val elevator = Elevator(F3)
    val items: Set[Item] = Set(
      Microchip('A', F4),
      Generator('A', F4),
      Microchip('B', F4),
      Generator('B', F4),
      Microchip('C', F4),
      Generator('C', F4)
    )

    // CALL + VERIFY
    State.isSolved(elevator, items) should be (false)

  }

  it should "see that state is not solved when any item is not at F4" in {

    // SETUP
    val elevator = Elevator(F4)
    val items: Set[Item] = Set(
      Microchip('A', F4),
      Generator('A', F4),

      Microchip('B', F3), // OH NOES!

      Generator('B', F4),
      Microchip('C', F4),
      Generator('C', F4)
    )

    // CALL + VERIFY
    State.isSolved(elevator, items) should be (false)

  }

  "Node" should "generate a tree" in {

    val x = State(Elevator(F1), Set(Microchip('A', F1)))



  }

}
