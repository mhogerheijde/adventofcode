package net.hogerheijde.aoc2016.days

abstract class RunnableDay {
  def run(): Unit
  final def main(args: Array[String]): Unit = run()
}
