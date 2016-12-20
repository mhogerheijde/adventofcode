package net.hogerheijde.aoc2016.days.day10

import scala.annotation.tailrec


case class Processor(states: IndexedSeq[State] = IndexedSeq()) {

  val state: State = states.lastOption.getOrElse(State(Set()))

  @tailrec
  final def resolve: Processor = {
    if (state.isDirty) {
      Processor(states :+ state.resolve).resolve
    } else {
      this
    }
  }

  def addInstruction(id: Int, instruction: Instruction): Processor = {
    val botId = BotId(id)

    val high: Target = state.findById(instruction.high).getOrElse(instruction.high.createNew)
    val low: Target = state.findById(instruction.low).getOrElse(instruction.low.createNew)

    val bot: Bot = state.bots.getOrElse(botId, Bot(id)) match {
      case bot: PartialBot => bot.copy(instruction = Some(instruction))
      case bot: FullBot => bot.copy(instruction = Some(instruction))
    }

    Processor(states :+ state.update(Set(bot, high, low))).resolve
  }

  def addChip(id: Int, chip: Microchip): Processor = {
    val botId = BotId(id)
    val newBot = state.bots.getOrElse(botId, Bot(id)) match {
      case bot: PartialBot => bot.addChip(chip)
      case bot: FullBot => throw new IllegalStateException(s"Cannot add a new chip to a full bot: trying to add $chip to $bot")
    }
    val newState = state.update(Set(newBot))
    Processor(states :+ newState).resolve
  }
}

object Processor {
  def withInitialState(state: State): Processor = {
    // TODO Check initial state for consistency
    Processor(IndexedSeq(state))
  }
}

case class State(targets: Set[Target]) {
  val bots: Map[BotId, Bot] = (targets collect { case bot: Bot => (bot.id, bot) }).toMap
  val outputs: Map[OutputId, Output] = (targets collect { case output: Output => (output.id, output) }).toMap

  def findById(target: TargetId): Option[Target] = {
    target match {
      case botId: BotId => bots.get(botId)
      case outputId: OutputId => outputs.get(outputId)
    }
  }

  def findBotById(bot: Bot): Option[Bot] = bots.get(bot.id)

  def findBot(chip1: Microchip, chip2: Microchip): Option[FullBot] = {
    bots collect { case (_, bot: FullBot) => bot } find { bot => bot.contains(chip1, chip2) }
  }

  def update(targets: Set[Target]): State = {
    val newBots = (targets collect { case b: Bot => (b.id, b) }).toMap
    val newOutputs = (targets collect { case o: Output => (o.id, o) }).toMap

    val newTargets: Set[Target] = (bots ++ newBots).values.toSet ++ (outputs ++ newOutputs).values.toSet
    State(newTargets)
  }


  def isDirty: Boolean = this.bots exists {
    case (_, bot: FullBot) => bot.instruction.isDefined
    case _ => false
  }

  final def resolve: State = {

    val botsThatNeedAction = (this.bots collect { case (_, bot: FullBot) if bot.instruction.isDefined => bot }).toSet

    if (botsThatNeedAction.isEmpty) {
      this
    } else {

      val newTargets = botsThatNeedAction flatMap { bot =>
        val instruction = bot.instruction.get
        val (low, high, tempBot) = bot.collect

        val lowTarget = this.findById(instruction.low).get match {
          case target: OpenSlot => target.addChip(low)
          case filledUp => throw new IllegalStateException(s"Cannot use target $filledUp, since it has no open slots.")
        }
        val highTarget = this.findById(instruction.high).get match {
          case target: OpenSlot => target.addChip(high)
          case filledUp => throw new IllegalStateException(s"Cannot use target $filledUp, since it has no open slots.")
        }

        val newBot = tempBot.copy(instruction = Some(instruction.copy(low = lowTarget.id, high = highTarget.id)))
        Set(lowTarget, highTarget, newBot)

      }

      this.update(newTargets)
    }
  }

}

case class Microchip(value: Int)


trait TargetId {
  def createNew: Target
}
case class BotId(id: Int) extends TargetId {
  override def createNew: PartialBot = Bot(id)
}
case class OutputId(id: Int) extends TargetId {
  override def createNew: Output = Output(id)
}


trait Target {
  def id: TargetId
}
trait OpenSlot {
  def addChip(chip: Microchip): Target
}
case class Output(id: OutputId, chips: IndexedSeq[Microchip] = IndexedSeq()) extends Target with OpenSlot {
  def addChip(chip: Microchip): Output = this.copy(chips = chips :+ chip)
}
object Output {
  def apply(id: Int): Output = Output(OutputId(id))
  def apply(id: Int, chips: IndexedSeq[Microchip]): Output = Output(OutputId(id), chips)
}

trait Bot extends Target {
  override def id: BotId
  def history: IndexedSeq[Set[Microchip]]
}
object Bot {
  def apply(id: Int): PartialBot = PartialBot(BotId(id), None, None, IndexedSeq())
  def apply(id: Int, instruction: Instruction): PartialBot = PartialBot(BotId(id), Some(instruction), None, IndexedSeq())
  def apply(id: Int, instruction: Instruction, chip: Microchip): PartialBot = PartialBot(BotId(id), Some(instruction), Some(chip), IndexedSeq())
  def apply(id: Int, instruction: Instruction, chip1: Microchip, chip2: Microchip): FullBot = FullBot(BotId(id), Some(instruction), chip1, chip2, IndexedSeq())
}
case class PartialBot(id: BotId,
                      instruction: Option[Instruction],
                      chip: Option[Microchip],
                      history: IndexedSeq[Set[Microchip]]) extends Bot with OpenSlot  {

  def addChip(chip: Microchip): Bot = {
    this match {
      case PartialBot(_, _, None, _) => this.copy(chip = Some(chip))
      case PartialBot(id, instruction, Some(chip1), history) => FullBot(id, instruction, chip1, chip, history)
    }
  }
}


case class FullBot(id: BotId,
                   instruction: Option[Instruction],
                   slot1: Microchip,
                   slot2: Microchip,
                   history: IndexedSeq[Set[Microchip]]) extends Bot {

  def collect: (Microchip, Microchip, PartialBot) = {
    val bot = PartialBot(id, instruction, None, history :+ Set(slot1, slot2))
    if (slot1.value < slot2.value) { (slot1, slot2, bot) } else { (slot2, slot1, bot) }
  }

  def contains(chip1: Microchip, chip2: Microchip): Boolean = {
    (chip1 == slot1 && chip2 == slot2) ||
      (chip1 == slot2 && chip2 == slot1)
  }

}


case class Instruction(low: TargetId, high: TargetId)
