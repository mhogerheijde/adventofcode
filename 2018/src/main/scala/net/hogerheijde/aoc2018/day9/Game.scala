package net.hogerheijde.aoc2018.day9

import net.hogerheijde.aoc2018.day9.Game.Score
import net.hogerheijde.aoc.util.Implicits.TextHelpers

import scala.collection.immutable.List


case class GameState(players: Map[Int, Int], circle: Marbles, currentPlayer: Int, maxPlayer: Int) {

  def next(nextMarble: Int): GameState = {
    val nextPlayer = if (currentPlayer + 1 > maxPlayer) { 1 } else { currentPlayer + 1}
    val (newMarbles, score) = circle.addMarble(nextMarble)
    val playerScore = players(nextPlayer)
    val newPlayers = players.updated(nextPlayer, playerScore + score)
    GameState(newPlayers, newMarbles, nextPlayer, maxPlayer)
  }

  override def toString: String = {
    val player = if (currentPlayer == 0) { "-" } else { currentPlayer.toString }

    val fillSize = players.keySet.max.toString.length
    val paddingSize = fillSize - player.length

    val p = "[" + (" " * paddingSize) + s"$player]"

    s"$p ${circle.toString}"
  }
}

case class Game(state: GameState, noOfMarbles: Int) {

  def hiScore: Int = state.players.values.max

  def solve: Game = {
//    println(s"Checking for $noOfMarbles")
    Range.inclusive(1, noOfMarbles).foldLeft(this) { case (game, nextMarble) =>
      if (nextMarble % 1000 == 0) { print(".") }
      if (nextMarble % (1000 * 100) == 0) {
        println(s"($nextMarble / $noOfMarbles) | ${Math.round(nextMarble.toDouble / noOfMarbles.toDouble * 10000.0) / 100.0}% ")
      }
      val newState = game.state.next(nextMarble)
      Game(newState, noOfMarbles)
    }
  }

  override def toString: String = state.toString
}
object Game {
  type Score = Int
  def start(noOfPlayers: Int, noOfMarbles: Int): Game = {
    val players = Range.inclusive(1, noOfPlayers).foldLeft(Map.empty[Int, Int]) {
      (result, currentPlayer) => result.updated(currentPlayer, 0)
    }
    val initialState = GameState(players, Marbles.cleanBoard, 0, noOfPlayers)
    Game(initialState, noOfMarbles)
  }
}


class Marbles(val left: List[Int], val right: List[Int]) {
//  val highestValue = marbles.max

//  val marbles = (left.reverse ++ right).toIndexedSeq

  def addMarble(nextMarble: Int): (Marbles, Score) = {
    if (nextMarble % 23 == 0) {
      val shift = left.take(8).reverse
      val newLeft = shift.take(2).last +: left.drop(8)
      val newRight = shift.drop(2) ++ right
      (Marbles(newLeft, newRight), nextMarble + shift.head)
    } else {
      if (right.isEmpty && left.size == 1) {
        (Marbles(nextMarble +: left, right), 0)
      } else if (right.isEmpty) {
        val r = left.reverse
        val l = List.empty[Int]
        val newLeft = nextMarble +: (r.head +: l)
        val newRight = r.tail
        (new Marbles(newLeft, newRight), 0)
      } else if (right.size == 1) {
        val mostLeft = left.last
        val r = right :+ mostLeft
        val l = left.dropRight(1)
        val newLeft = nextMarble +: (r.head +: l)
        val newRight = r.tail
        (new Marbles(newLeft, newRight), 0)
      } else {
        val newLeft = nextMarble +: (right.head +: left)
        val newRight = right.tail
        (new Marbles(newLeft, newRight), 0)
      }
    }
  }

  override def hashCode(): Score = left.hashCode() + right.hashCode()
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Marbles => {
        if (this.left == other.left && this.right == other.right) {
          true
        } else {
          this.left ++ this.right.reverse == other.left ++ other.right.reverse
        }
      }
      case _ => false
    }
  }

  override def toString: String = {
    val b = new StringBuilder()
    for (l <- left.drop(1).reverse) {
      b append s"$l "
    }
    b append s"(${left.head}) "
    for (r <- right) {
      b append s"$r "
    }
    b.toString
  }
}
object Marbles {

  def apply(left: List[Score], right: List[Score]): Marbles = new Marbles(left, right)
  def apply(marbles: IndexedSeq[Score], currentIndex: Score): Marbles =
    new Marbles(marbles.take(currentIndex + 1).reverse.toList, marbles.drop(currentIndex + 1).toList)

  val cleanBoard: Marbles = Marbles(List(0), List.empty[Int])
}


//class LinkedList[T] private (next: LinkedList.Node[T], last: LinkedList.Node[T]) {
//  def prePend(value: T): LinkedList[T] = {
//    new LinkedList(new LinkedList.Node(Some(this.next), value), last)
//  }
//}
//object LinkedList {
//
//  def apply[T](value: T): LinkedList[T] = {
//    val node = new LinkedList.Node(None, value)
//    new LinkedList(node, node)
//  }
//
//  class Node[T](next: Option[Node[T]], value: T)
//}

