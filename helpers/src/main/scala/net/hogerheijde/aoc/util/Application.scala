package net.hogerheijde.aoc.util

trait Application {
  def year: String

  // FIXME figure out how to do reflection....
  def days: Seq[Runnable]

  def main(args: Array[String]): Unit = {
    printHeader()

    days.foreach(_.run)
  }

  def printHeader(): Unit = {
    println("""
              |   __    ____  _  _  ____  _  _  ____    _____  ____     ___  _____  ____  ____
              |  /__\  (  _ \( \/ )( ___)( \( )(_  _)  (  _  )( ___)   / __)(  _  )(  _ \( ___)
              | /(__)\  )(_) )\  /  )__)  )  (   )(     )(_)(  )__)   ( (__  )(_)(  )(_) ))__)
              |(__)(__)(____/  \/  (____)(_)\_) (__)   (_____)(__)     \___)(_____)(____/(____)
              |""".stripMargin + s"\n $year")
    println()
  }
}
