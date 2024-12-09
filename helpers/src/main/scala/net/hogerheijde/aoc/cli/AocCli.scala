package net.hogerheijde.aoc.cli

import org.rogach.scallop._

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val apples = opt[Int](required = true)
  verify()
}

object AocCli:
  def main(args: Array[String]) =
    val conf = new Conf(args.toSeq)
    println("apples are: " + conf.apples())
