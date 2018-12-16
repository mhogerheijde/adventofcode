package net.hogerheijde.aoc2018.day13

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc2018.day13.Model
import net.hogerheijde.aoc2018.day13.Model.BottomLeft
import net.hogerheijde.aoc2018.day13.Model.BottomRight
import net.hogerheijde.aoc2018.day13.Model.Cart
import net.hogerheijde.aoc2018.day13.Model.Crossing
import net.hogerheijde.aoc2018.day13.Model.East
import net.hogerheijde.aoc2018.day13.Model.Grid
import net.hogerheijde.aoc2018.day13.Model.Left
import net.hogerheijde.aoc2018.day13.Model.Right
import net.hogerheijde.aoc2018.day13.Model.Horizontal
import net.hogerheijde.aoc2018.day13.Model.North
import net.hogerheijde.aoc2018.day13.Model.South
import net.hogerheijde.aoc2018.day13.Model.Space
import net.hogerheijde.aoc2018.day13.Model.Straight
import net.hogerheijde.aoc2018.day13.Model.TopRight
import net.hogerheijde.aoc2018.day13.Model.TopLeft
import net.hogerheijde.aoc2018.day13.Model.Vertical
import net.hogerheijde.aoc2018.day13.Model.West
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day13Test extends WordSpec with Matchers {



  val part2ExampleGrid = Grid.parse(
    """/>-<\
      #|   |
      #| /<+-\
      #| | | v
      #\>+</ |
      #  |   ^
      #  \<->/""".stripMargin('#'))


  val exampleTrack =
    """/->-\
      #|   |  /----\
      #| /-+--+-\  |
      #| | |  | v  |
      #\-+-/  \-+--/
      #  \------/""".stripMargin('#')

  private val exampleTrackMap: Map[Coordinate, Model.TrackType] = Map(
    Coordinate(0, 0) -> BottomRight,
    Coordinate(1, 0) -> Horizontal,
    Coordinate(2, 0) -> Horizontal,
    Coordinate(3, 0) -> Horizontal,
    Coordinate(4, 0) -> BottomLeft,

    Coordinate(0, 1) -> Vertical,
    Coordinate(1, 1) -> Space,
    Coordinate(2, 1) -> Space,
    Coordinate(3, 1) -> Space,
    Coordinate(4, 1) -> Vertical,
    Coordinate(5, 1) -> Space,
    Coordinate(6, 1) -> Space,
    Coordinate(7, 1) -> BottomRight,
    Coordinate(8, 1) -> Horizontal,
    Coordinate(9, 1) -> Horizontal,
    Coordinate(10, 1) -> Horizontal,
    Coordinate(11, 1) -> Horizontal,
    Coordinate(12, 1) -> BottomLeft,

    Coordinate(0, 2) -> Vertical,
    Coordinate(1, 2) -> Space,
    Coordinate(2, 2) -> BottomRight,
    Coordinate(3, 2) -> Horizontal,
    Coordinate(4, 2) -> Crossing,
    Coordinate(5, 2) -> Horizontal,
    Coordinate(6, 2) -> Horizontal,
    Coordinate(7, 2) -> Crossing,
    Coordinate(8, 2) -> Horizontal,
    Coordinate(9, 2) -> BottomLeft,
    Coordinate(10, 2) -> Space,
    Coordinate(11, 2) -> Space,
    Coordinate(12, 2) -> Vertical,

    Coordinate(0, 3) -> Vertical,
    Coordinate(1, 3) -> Space,
    Coordinate(2, 3) -> Vertical,
    Coordinate(3, 3) -> Space,
    Coordinate(4, 3) -> Vertical,
    Coordinate(5, 3) -> Space,
    Coordinate(6, 3) -> Space,
    Coordinate(7, 3) -> Vertical,
    Coordinate(8, 3) -> Space,
    Coordinate(9, 3) -> Vertical,
    Coordinate(10, 3) -> Space,
    Coordinate(11, 3) -> Space,
    Coordinate(12, 3) -> Vertical,

    Coordinate(0, 4) -> TopRight,
    Coordinate(1, 4) -> Horizontal,
    Coordinate(2, 4) -> Crossing,
    Coordinate(3, 4) -> Horizontal,
    Coordinate(4, 4) -> TopLeft,
    Coordinate(5, 4) -> Space,
    Coordinate(6, 4) -> Space,
    Coordinate(7, 4) -> TopRight,
    Coordinate(8, 4) -> Horizontal,
    Coordinate(9, 4) -> Crossing,
    Coordinate(10, 4) -> Horizontal,
    Coordinate(11, 4) -> Horizontal,
    Coordinate(12, 4) -> TopLeft,

    Coordinate(0, 5) -> Space,
    Coordinate(1, 5) -> Space,
    Coordinate(2, 5) -> TopRight,
    Coordinate(3, 5) -> Horizontal,
    Coordinate(4, 5) -> Horizontal,
    Coordinate(5, 5) -> Horizontal,
    Coordinate(6, 5) -> Horizontal,
    Coordinate(7, 5) -> Horizontal,
    Coordinate(8, 5) -> Horizontal,
    Coordinate(9, 5) -> TopLeft,
  )
  val exampleGrid = Grid(
    exampleTrackMap,
    Map(
      Coordinate(2, 0) -> Cart(Coordinate(2, 0), East, Right),
      Coordinate(9, 3) -> Cart(Coordinate(9, 3), South, Right),
    )
  )


  "Grid" should {
    "parse example" in {

      val parsedGrid = Grid.parse(exampleTrack)
      parsedGrid should be (exampleGrid)


    }

    "calculate next" in {
      val gen1 = exampleGrid.next.left.get
      val gen2 = gen1.next.left.get
      val gen3 = gen2.next.left.get
      val gen4 = gen3.next.left.get
      val gen5 = gen4.next.left.get
      val gen6 = gen5.next.left.get
      val gen7 = gen6.next.left.get
      val gen8 =  gen7.next.left.get
      val gen9 =  gen8.next.left.get
      val gen10 = gen9.next.left.get
      val gen11 = gen10.next.left.get
      val gen12 = gen11.next.left.get
      val gen13 = gen12.next.left.get

      gen1 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(3, 0) -> Cart(Coordinate(3, 0), East, Right),
            Coordinate(9, 4) -> Cart(Coordinate(9, 4), East, Left),
          )
        )
      )

      gen2 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(4, 0) -> Cart(Coordinate(4, 0), South, Right),
            Coordinate(10, 4) -> Cart(Coordinate(10, 4), East, Left),
          )
        )
      )


      gen3 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(4, 1) -> Cart(Coordinate(4, 1), South, Right),
            Coordinate(11, 4) -> Cart(Coordinate(11, 4), East, Left),
          )
        )
      )

      gen4 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(4, 2) -> Cart(Coordinate(4, 2), East, Left),
            Coordinate(12, 4) -> Cart(Coordinate(12, 4), North, Left),
          )
        )
      )



      /*
/---\
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/
       */

      gen7 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(7, 2) -> Cart(Coordinate(7, 2), East, Straight),
            Coordinate(12, 1) -> Cart(Coordinate(12, 1), West, Left),
          )
        )
      )

/*
/---\
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/
 */
      gen13 should be (
        Grid(
          exampleTrackMap,
          Map(
            Coordinate(7, 2) -> Cart(Coordinate(7, 2), South, Straight),
            Coordinate(7, 4) -> Cart(Coordinate(7, 4), North, Right),
          )
        )
      )


    }

    "find collision" in {
      exampleGrid.collision should be(Coordinate(7, 3))
    }

    "find next by removing carts" in {
      val gen1 = part2ExampleGrid.safeNext

      gen1 should be (
        part2ExampleGrid.copy(carts = Map(
          Coordinate(2, 2) -> Cart(Coordinate(2, 2), South, Right),
          Coordinate(2, 6) -> Cart(Coordinate(2, 6), North, Right),
          Coordinate(6, 6) -> Cart(Coordinate(6, 6), North, Right),
        ))
      )

      val gen2 = gen1.safeNext
      gen2 should be (
        part2ExampleGrid.copy(carts = Map(
          Coordinate(2, 3) -> Cart(Coordinate(2, 3), South, Right),
          Coordinate(2, 5) -> Cart(Coordinate(2, 5), North, Right),
          Coordinate(6, 5) -> Cart(Coordinate(6, 5), North, Right),
        ))
      )

      val gen3 = gen2.safeNext
      gen3 should be (
        part2ExampleGrid.copy(carts = Map(
          Coordinate(6, 4) -> Cart(Coordinate(6, 4), North, Right),
        ))
      )
    }


    "find last cart" in {
      part2ExampleGrid.lastRemaining() should be (Coordinate(6, 4))
    }
  }
}
