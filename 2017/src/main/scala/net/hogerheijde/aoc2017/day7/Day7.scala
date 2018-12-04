package net.hogerheijde.aoc2017.day7

import net.hogerheijde.aoc2017.day7.Disc.DiscsByName
import net.hogerheijde.aoc2017.day7.Disc.TowersByDisc
import scala.collection.immutable.IndexedSeq

import net.hogerheijde.aoc2017.Day2017

object Day7 extends Day2017[(TowersByDisc, DiscsByName), String, Int] {
  override def name: String = "Day 7"



  override def parse(input: String): (TowersByDisc, DiscsByName) = {
    val pass1 = input.lines.map { line =>
      val parts = line.split(" -> ")
      val disc: Disc = Disc.fromString(parts(0))

      val carrying = if (parts.length > 1) {
        parts(1).split(", ").toIndexedSeq.map(_.trim)
      } else { IndexedSeq() }

      (disc, carrying)
    }.toIndexedSeq

    val disksByName = pass1.map { case (disc, _) => (disc.name, disc) }.toMap

    val towersByDisc = pass1.map { case (disc, carrying) =>
      val discsMapped = carrying.map { name =>
        disksByName(name)
      }
      (disc, discsMapped)
    }.toMap

    (towersByDisc, disksByName)

  }
  override def part1(input: (TowersByDisc, DiscsByName)): String = {

    val outgoingRelationships = input._1.foldLeft(Set.empty[(String, String)]) { case (relations, (disc, carrying)) =>
        val currentRelations = carrying.map( carried => (disc.name, carried.name) ).toSet
        relations ++ currentRelations
    }

    val (carryingDiscs, discsBeingCarried) = outgoingRelationships.unzip

    val rootDiscs = carryingDiscs -- discsBeingCarried

    if (rootDiscs.size == 1) {
      rootDiscs.head
    } else {
      throw new RuntimeException(s"Something went wrong; got these rootdisks: $rootDiscs")
    }


  }
  override def part2(input: (TowersByDisc, DiscsByName)): Int = {
    val root = part1(input)

    val rootDisc = input._2(root)

    val result = getBalance(input._1)(rootDisc)

    result match {
      case Left(inbalance) => inbalance._1.weight + inbalance._2
      case _ => throw new RuntimeException("There was no inbalance?")
    }
  }


  def getWeight(towersByDisc: TowersByDisc)(root: Disc): Int = {
    towersByDisc(root).map(getWeight(towersByDisc)).sum + root.weight
  }


  def getBalance(towersByDisc: TowersByDisc)(root: Disc): Either[(Disc, Int), (Disc, Int)] = {

    val towers = towersByDisc(root)

    towers match {
      case IndexedSeq() => Right((root, root.weight))
      case _ =>
        val recursive = towers.map(getBalance(towersByDisc))

        val nonBalance = recursive.collect {  case Left(i) => i }
        val balance = recursive.collect { case Right(i) => i }

        if (balance.map(_._2).distinct.length == 1 && nonBalance.isEmpty) {
          val towerWeight = balance.foldLeft(0) { (t, d) => t + d._2 } + root.weight
          val result = Right((root, towerWeight))
          result
        } else if (nonBalance.nonEmpty) {
          val result = Left(nonBalance.head)
          result
        } else {
          val groupedByWeight = balance.groupBy(_._2)
          val oddOneOut = groupedByWeight.find(things => things._2.length == 1 ).get
          val consensus = groupedByWeight.find(things => things._2.length != 1 ).get
          val result = Left( (oddOneOut._2.head._1, consensus._1 - oddOneOut._1 ) )
          result
        }
    }
  }
}



