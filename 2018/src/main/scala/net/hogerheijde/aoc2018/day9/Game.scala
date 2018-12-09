package net.hogerheijde.aoc2018.day9

import net.hogerheijde.aoc2018.day9.Game.Score


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
    println(s"Checking for $noOfMarbles")
    Range.inclusive(1, noOfMarbles).foldLeft(this) { case (game, nextMarble) =>
      if (nextMarble % 100 == 99) { print(".") }
      if (nextMarble % (100 * 100) == 0) {
        println(s"($nextMarble / $noOfMarbles) | ${Math.round(nextMarble.toDouble / noOfMarbles.toDouble * 10000.0) / 100.0}% ") }
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


case class Marbles(marbles: IndexedSeq[Int], currentIndex: Int) {
//  val highestValue = marbles.max

  def addMarble(nextMarble: Int): (Marbles, Score) = {
    if (nextMarble % 23 == 0) {
      val offset = currentIndex - 7
      val newCurrentIndex = if (offset < 0) { marbles.length + offset } else { offset }
      val marbleToBeRemoved = marbles(newCurrentIndex)
      val newMarbles = marbles.take(newCurrentIndex) ++ marbles.drop(newCurrentIndex + 1)
      (Marbles(newMarbles, newCurrentIndex), nextMarble + marbleToBeRemoved)
    } else {
      val insertAfter = ((currentIndex + 1) % marbles.length) + 1
      val newMarbles = (marbles.take(insertAfter) :+ nextMarble) ++ marbles.drop(insertAfter)
      (Marbles(newMarbles, insertAfter), 0)
    }
  }


  override def toString: String = {
    val current = marbles(currentIndex)
    val b = new StringBuilder()
    for (m <- marbles) {
      b append (if (m==current) {
        val inBold = s"${m.toString.map { c => Marbles.Bold(c.toString.toInt) }.mkString("")}"
        s"($inBold) "
      } else {
        s"$m "
      })
    }
    b.toString
  }
}
object Marbles {
  private val Bold = IndexedSeq("𝟬", "𝟭", "𝟮", "𝟯", "𝟰", "𝟱", "𝟲", "𝟳", "𝟴", "𝟵")
  val cleanBoard: Marbles = Marbles(IndexedSeq(0), 0)
}