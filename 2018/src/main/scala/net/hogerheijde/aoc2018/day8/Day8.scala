package net.hogerheijde.aoc2018.day8

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2018.Day2018
import scala.collection.immutable.IndexedSeq
import scala.util.Try

object Day8 extends Day2018[Node, Int, Int] {
  override def name: String = "Day 8"

  override def parse(input: String): Node =
    Node.readNode(Parser.standardIntSplit(input))._1

  override def part1(input: Node): Int = input.metadataSum

  override def part2(input: Node): Int = input.value
}



case class Node(metadata: IndexedSeq[Int], children: IndexedSeq[Node]) {
  def value: Int = {
    if (children.isEmpty) {
      metadata.sum
    } else {
      val mapping = metadata.map( index =>
        Try(children(index-1).value).getOrElse(0)
      )
      mapping.sum
    }
  }
  def count: Int = 1 + children.map(_.count).sum
  def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum
}


object Node {

  def apply(metadata: IndexedSeq[Int], children: Node*): Node = Node(metadata, children.toIndexedSeq)
  def fromMetadata(metadata: IndexedSeq[Int]): Node = new Node(metadata, IndexedSeq())
  def fromMetadata(metadata: Int*): Node = new Node(metadata.toIndexedSeq, IndexedSeq())

  def readNode(input: IndexedSeq[Int]): (Node, IndexedSeq[Int]) = {


    val childrenCount = input.head
    val metadataCount = input.tail.head


    if (childrenCount == 0) {
      val metadata = input.drop(2).take(metadataCount)
      val remainder = input.drop(2 + metadataCount)
      (Node.fromMetadata(metadata), remainder)
    } else {
      // We do have children
      val start = (IndexedSeq.empty[Node], input.drop(2))
      val (children, remainder) = Range(0, childrenCount).foldLeft(start) { case ((children, remainder), _) =>
        val (nextChild, newRemainder) = readNode(remainder)
        (children :+ nextChild, newRemainder)
      }

      val metadata = remainder.take(metadataCount)
      val remainder2 = remainder.drop(metadataCount)
      (Node(metadata, children), remainder2)
    }


  }
}