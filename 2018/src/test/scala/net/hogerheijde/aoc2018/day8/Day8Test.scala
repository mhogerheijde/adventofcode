package net.hogerheijde.aoc2018.day8

import org.scalatest.Matchers
import org.scalatest.WordSpec

import scala.collection.immutable.IndexedSeq

class Day8Test extends WordSpec with Matchers {

  val exampleNode_D = Node.fromMetadata(99)
  val exampleNode_B = Node.fromMetadata(10, 11, 12)

  val exampleNode_C = Node(
    IndexedSeq(2), IndexedSeq(
      exampleNode_D
    ))

  val exampleNode_A =
    Node(IndexedSeq(1, 1, 2),
      exampleNode_B,
      exampleNode_C,
    )

  "Day 8" should {

    "parse example" in {
      Day8.parse("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2") should be(exampleNode_A)
    }
  }
  "Node" should {

    "read single node" in {
      Node.readNode(IndexedSeq(0, 2, 11, 12)) should be (
        Node.fromMetadata(11, 12),
        IndexedSeq()
      )
    }

    "read sibling nodes with no children" in {
      Node.readNode(IndexedSeq(0, 2, 11, 12, 0, 3, 13, 14, 15)) should be (
        Node.fromMetadata(11, 12),
        IndexedSeq(0, 3, 13, 14, 15),
      )
    }

    "read node with child" in {
      Node.readNode(IndexedSeq(1, 2, 0, 3, 20, 21, 22, 11, 12)) should be (
        (
          Node(IndexedSeq(11, 12),
            Node.fromMetadata(20, 21, 22)
          ),
          IndexedSeq()
        )
      )
    }

    "read example node" in {
      Node.readNode(IndexedSeq(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)) should be (
        (exampleNode_A, IndexedSeq())
      )
    }

    "calcluate metadata sum" in {
      exampleNode_A.metadataSum should be (138)
    }

    "calculate value for node without children" in {
      exampleNode_B.value should be (33)
      exampleNode_D.value should be (99)
    }

    "calculate value for node with children" in {
      exampleNode_C.value should be (0)
      exampleNode_A.value should be (66)
    }

  }


}
