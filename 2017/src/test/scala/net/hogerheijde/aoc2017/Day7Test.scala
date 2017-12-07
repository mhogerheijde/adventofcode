package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day7.Day7
import net.hogerheijde.aoc2017.day7.Disc
import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq
import scala.io.Source

class Day7Test extends WordSpec with Matchers {

  val inputSmall =
    """
      |xhth (51)
      |ktlj (52)
      |fwft (72) -> ktlj, cntj, xhth
      |cntj (53)
    """.stripMargin.trim

  val input =
    """
      |pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
    """.stripMargin.trim

  val parsedInput = Day7.parse(input)


  "Day7" should {
    "parse input" in {

      val expect = Map(
        Disc("xhth", 51) -> IndexedSeq(),
        Disc("ktlj", 52) -> IndexedSeq(),
        Disc("fwft", 72) -> IndexedSeq(
          Disc("ktlj", 52),
          Disc("cntj", 53),
          Disc("xhth", 51)
        ),
        Disc("cntj", 53) -> IndexedSeq()
      )
      Day7.parse(inputSmall)._1 should be(expect)
    }


    "find root" in {
      Day7.part1(Day7.parse(inputSmall)) should be ("fwft")
      Day7.part1(Day7.parse(input)) should be ("tknk")
    }

    "find weight of example" in {

      val towersByDisc = parsedInput._1
      val discByName = parsedInput._2

      Day7.getWeight(towersByDisc)(discByName("ugml")) should be(251)
      Day7.getWeight(towersByDisc)(discByName("padx")) should be(243)
      Day7.getWeight(towersByDisc)(discByName("fwft")) should be(243)

    }

    "find inbalance small example" in {

      val input = Map(
        Disc("xhth", 11) -> IndexedSeq(),
        Disc("ktlj", 10) -> IndexedSeq(),
        Disc("fwft", 1) -> IndexedSeq(
          Disc("ktlj", 10),
          Disc("cntj", 10),
          Disc("xhth", 11)
        ),
        Disc("cntj", 10) -> IndexedSeq()
      )


      Day7.getBalance(input)(Disc("fwft", 1)) should be (Left(Disc("xhth", 11), -1))

    }

    "find inbalance example" in {

      Day7.getBalance(Day7.parse(input)._1)(Disc("tknk", 41)) should be (Left(Disc("ugml", 68), -8))

    }

    "find inbalance in ptshtrn" in {
      val input = {
        Option(Source.fromResource(s"net/hogerheijde/aoc2017/day7.input")) match {
          case Some(source) => source.mkString.trim
          case None => throw new RuntimeException("Did you forget to place the puzzle input in the resources folder?")
        }
      }

      val data = Day7.parse(input)

      val towersByDisc = data._1
      val discByName = data._2

      val w = Day7.getWeight(towersByDisc)(discByName("ptshtrn"))
      val w1 = Day7.getWeight(towersByDisc)(discByName("uslizt"))
      val w2 = Day7.getWeight(towersByDisc)(discByName("pjqtv"))
      val w3 = Day7.getWeight(towersByDisc)(discByName("bzbdorp"))
      val w4 = Day7.getWeight(towersByDisc)(discByName("atvkttc"))

      val r1 = Day7.getWeight(towersByDisc)(discByName("ptshtrn"))
      val r2 = Day7.getWeight(towersByDisc)(discByName("mxgpbvf"))
      val r3 = Day7.getWeight(towersByDisc)(discByName("cfqdsb"))
      val r4 = Day7.getWeight(towersByDisc)(discByName("yfejqb"))



      Day7.getBalance(towersByDisc)(discByName("ptshtrn")) should be (Right(Disc("ptshtrn", 526), 1122))
      Day7.getBalance(towersByDisc)(discByName("mxgpbvf")) should be (Right(Disc("mxgpbvf", 52), 1117))
      Day7.getBalance(towersByDisc)(discByName("cfqdsb")) should be (Right(Disc("cfqdsb", 556), 1117))
      Day7.getBalance(towersByDisc)(discByName("yfejqb")) should be (Right(Disc("yfejqb", 493), 1117))

      Day7.getBalance(towersByDisc)(discByName("nieyygi")) should be (Left(Disc("ptshtrn", 526), -5))


    }


  }

}
