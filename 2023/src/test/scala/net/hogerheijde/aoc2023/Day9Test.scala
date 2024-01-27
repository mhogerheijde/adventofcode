package net.hogerheijde.aoc2023

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2023.Day9.layers
import net.hogerheijde.aoc2023.Day9.nextLayer
import net.hogerheijde.aoc2023.Day9.predict
import net.hogerheijde.aoc2023.Day9.predict2
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with Matchers {

  def lSeq(i: Int*): Seq[Long] = i.map(_.toLong).toSeq

  val exampleInput: String =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".stripMargin

  val exampleModel = Seq(
    Seq(0, 3, 6, 9, 12, 15),
    Seq(1, 3, 6, 10, 15, 21),
    Seq(10, 13, 16, 21, 30, 45),
  )

  "Day 9" should {

    "parse input" in {
      Day9.parse(exampleInput) should be(exampleModel)
    }

    "predict previous" in {
      predict2(layers(Seq(0, 3, 6, 9, 12, 15))) should be(-3)
      predict2(layers(Seq(1, 3, 6, 10, 15, 21))) should be(0)
      predict2(layers(Seq(10, 13, 16, 21, 30, 45))) should be(5)
    }

    "predict next" in {
      predict(layers(Seq(0, 3, 6, 9, 12, 15))) should be (18)
      predict(layers(Seq(1, 3, 6, 10, 15, 21))) should be (28)
      predict(layers(Seq(10, 13, 16, 21, 30, 45))) should be (68)
      predict(layers(
        Seq(-5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25)
      )) should be (-26)


      val x1 =
        lSeq(0, -10, -20, -25, -20, 0, 40, 105, 200, 330, 500, 715, 980, 1300, 1680, 2125, 2640, 3230, 3900, 4655, 5500)
      val x2 = lSeq(-10, -10, -5, 5, 20, 40, 65, 95, 130, 170, 215, 265, 320, 380, 445, 515, 590, 670, 755, 845)
      val x3 = lSeq(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
      val x4 = lSeq(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
      predict(Seq(x4, x3)) should be (95)
      predict(Seq(x4, x3, x2)) should be (940)
      predict(Seq(x4, x3, x2, x1)) should be (6440)
    }

    "produce layers" in {
      layers(Seq(0, 3, 6, 9, 12, 15)) should be (
        Seq(
          Seq(3, 3, 3, 3, 3),
          Seq(0, 3, 6, 9, 12, 15),
        )
      )
      layers(Seq(1, 3, 6, 10, 15, 21)).map(_.last) should be(Seq(1, 6, 21))
      layers(Seq(1, 3, 6, 10, 15, 21)).map(_.head) should be(Seq(1, 2, 1))
    }

    "produce next layer" in {
      nextLayer(Seq(0, 3, 6, 9, 12, 15)) should be (Seq(3, 3, 3, 3, 3))
      nextLayer(Seq(3, 3, 3, 3, 3)) should be (Seq(0, 0, 0, 0))

      nextLayer(Seq(1, 3, 6, 10, 15, 21)) should be (Seq(2, 3, 4, 5, 6))
      nextLayer(Seq(2, 3, 4, 5, 6)) should be (Seq(1, 1, 1, 1))
      nextLayer(Seq(1, 1, 1, 1)) should be (Seq(0, 0, 0))

      nextLayer(
        Seq(-5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25)
      ) should be (
        Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      )

      val x1 =
        lSeq(0, -10, -20, -25, -20, 0, 40, 105, 200, 330, 500, 715, 980, 1300, 1680, 2125, 2640, 3230, 3900, 4655, 5500)
      val x2 = lSeq(-10, -10, -5, 5, 20, 40, 65, 95, 130, 170, 215, 265, 320, 380, 445, 515, 590, 670, 755, 845)
      val x3 = lSeq(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
      val x4 = lSeq(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
      val x5 = lSeq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      nextLayer(x1) should be (x2)
      nextLayer(x2) should be (x3)
      nextLayer(x3) should be (x4)
      nextLayer(x4) should be (x5)
    }

    "Part1: example answer" in {
      Day9.part1(Day9.parse(exampleInput)) should be(114)
    }

    "Part2: example answer" in {
      Day9.part2(Day9.parse(exampleInput)) should be(2)
    }
  }
}