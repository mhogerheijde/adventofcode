package net.hogerheijde.aoc2016.days.day10

import net.hogerheijde.aoc2016.Util
import net.hogerheijde.aoc2016.days.RunnableDay

object Day10 extends RunnableDay {

  def run(): Unit = {
    val input = Util.readFile("net/hogerheijde/aoc2016/days/day10.input")

    val result = processPt1(input)
    println(s"Day 10 - pt1: $result (expect ??)")
    //    val result2 = Day10.processPt2(input)
    //    println(s"Day 10 - pt2: $result2 (expect 11658395076)")
  }


  def processPt1(input: String): Option[Bot] = {
    val proc = parse(input).resolve
    println(proc.state.toDot())

//    val result = Processor.resolve(proc.state, IndexedSeq())

    val x = proc.states.flatMap(state => state.bots filter {
      case (_, b: FullBot) => b.slot1 == Microchip(61) || b.slot2 == Microchip(61)
      case _ => false
    })

    val y = proc.states.flatMap(state => state.bots filter {
      case (_, b: FullBot) => b.slot1 == Microchip(17) || b.slot2 == Microchip(17)
      case _ => false
    })

    val botsx = proc.state.bots.filter { case(id, bot) => bot.history.exists(item => item.contains(Microchip(61))) }
    val botsy = proc.state.bots.filter { case(id, bot) => bot.history.exists(item => item.contains(Microchip(17))) }

    proc.states.flatMap( state => state.findBot(Microchip(61), Microchip(17))).headOption
  }

  def parse(input: String): Processor = {
    input.split("\n").foldLeft(new Processor) { case (processor, nextLine) => {
        updateProcessor(nextLine, processor)
      }
    }
  }

  val instructionBotBot =       """bot ([0-9]+) gives low to bot ([0-9]+) and high to bot ([0-9]+)""".r
  val instructionBotOutput =    """bot ([0-9]+) gives low to bot ([0-9]+) and high to output ([0-9]+)""".r
  val instructionOutputBot =    """bot ([0-9]+) gives low to output ([0-9]+) and high to bot ([0-9]+)""".r
  val instructionOutputOutput = """bot ([0-9]+) gives low to output ([0-9]+) and high to output ([0-9]+)""".r
  val microchip =               """value ([0-9]+) goes to bot ([0-9]+)""".r
  def updateProcessor(line: String, processor: Processor): Processor = {
    line match {
      case instructionBotBot(botId, lowBotId, highBotId) =>
        processor.addInstruction(botId.toInt, Instruction(BotId(lowBotId.toInt), BotId(highBotId.toInt)))
      case instructionBotOutput(botId, lowBotId, highOutputId) =>
        processor.addInstruction(botId.toInt, Instruction(BotId(lowBotId.toInt), OutputId(highOutputId.toInt)))
      case instructionOutputBot(botId, lowOutputId, highBotId) =>
        processor.addInstruction(botId.toInt, Instruction(OutputId(lowOutputId.toInt), BotId(highBotId.toInt)))
      case instructionOutputOutput(botId, lowOutputId, highOutputId) =>
        processor.addInstruction(botId.toInt, Instruction(OutputId(lowOutputId.toInt), OutputId(highOutputId.toInt)))
      case microchip(chipValue, botId) =>
        processor.addChip(botId.toInt, Microchip(chipValue.toInt))
      case _ =>
        throw new IllegalArgumentException(s"Cannot parse line $line.")
    }
  }

}
