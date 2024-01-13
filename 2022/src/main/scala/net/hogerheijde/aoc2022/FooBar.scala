package net.hogerheijde.aoc2022

val d: Seq[Char] = Seq('⠷', '⠯', '⠟', '⠻', '⠽', '⠾').reverse

@main def main(): Unit =
  var i = 0;
  while (true)
    val c: Char = d(i)
    System.out.print("\r" + c)
    System.out.flush()
    i = (i + 1) % d.size
    Thread.sleep(150)
