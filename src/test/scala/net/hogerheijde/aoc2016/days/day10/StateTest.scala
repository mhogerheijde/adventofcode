package net.hogerheijde.aoc2016.days.day10

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class StateTest extends FlatSpec with Matchers {

  "State" should "update correctly" in {

    val emptyBot1 = Bot(1)
    val filledBot1  = Bot(1, Instruction(OutputId(0), OutputId(1)), Microchip(1))
    val emptyOutput1 = Output(1)
    val filledOutput1 = Output(1, IndexedSeq(Microchip(1)))
    val newBot = Bot(2)
    val newOutput = Output(2)

    val start = State(Set(emptyOutput1, emptyBot1))

    start.update(Set(filledBot1)) should be (State(Set(emptyOutput1, filledBot1)))
    start.update(Set(filledOutput1)) should be (State(Set(filledOutput1, emptyBot1)))
    start.update(Set(filledBot1, filledOutput1)) should be (State(Set(filledOutput1, filledBot1)))

    start.update(Set(newBot)) should be (State(Set(emptyOutput1, emptyBot1, newBot)))
    start.update(Set(newOutput)) should be (State(Set(emptyOutput1, emptyBot1, newOutput)))
    start.update(Set(newBot, newOutput)) should be (State(Set(emptyOutput1, emptyBot1, newOutput, newBot)))

  }

  it should "resolve correctly" in {

    // SETUP
    val startState= State(
      Set(
        Bot(1, Instruction(OutputId(1), OutputId(2)), Microchip(1), Microchip(2)),
        Output(1),
        Output(2)
      )
    )

    val expectedState = State(Set(
      PartialBot(BotId(1), Some(Instruction(OutputId(1), OutputId(2))), None, IndexedSeq(Set(Microchip(1), Microchip(2)))),
      Output(1, IndexedSeq(Microchip(1))),
      Output(2, IndexedSeq(Microchip(2)))
    ))

    // CALL
    val result = startState.resolve

    // VERIFY
    result should be (expectedState)

  }

  "FullBot" should "give correct low / high microchip" in {
    val bot1 = Bot(1, Instruction(OutputId(0), OutputId(1)), Microchip(1), Microchip(2))
    val bot2 = Bot(2, Instruction(OutputId(0), OutputId(1)), Microchip(3), Microchip(2))

    bot1.collect should be( (Microchip(1), Microchip(2), PartialBot(bot1.id, bot1.instruction, None, IndexedSeq(Set(Microchip(1), Microchip(2))))))
    bot2.collect should be( (Microchip(2), Microchip(3), PartialBot(bot2.id, bot2.instruction, None, IndexedSeq(Set(Microchip(2), Microchip(3))))))
  }

  "Processor" should "add a new Bot and Output" in  {
    new Processor().addInstruction(1, Instruction(BotId(2), OutputId(1))).state should be (
      State(Set(
        Bot(1, Instruction(BotId(2), OutputId(1))), Bot(2),
        Output(1)
      ))
    )
  }

  it should "update an existing Bot and leave Output as is" in {
    // SETUP
    val start = Processor.withInitialState(State(Set(
      Bot(1, Instruction(OutputId(0), OutputId(1)), Microchip(1)),
      Output(1, IndexedSeq(Microchip(2)))
    )))

    // CALL
    val result = start.addInstruction(1, Instruction(BotId(2), OutputId(1)))

    // VERIFY
    result.state should be(
      State(
        Set(
          Bot(1, Instruction(BotId(2), OutputId(1)), Microchip(1)),
          Bot(2),
          Output(1, IndexedSeq(Microchip(2)))
        )
      )
    )
  }

  it should "add chip to a new bot" in {
    // SETUP
    val start = new Processor()

    // CALL
    val result = start.addChip(1, Microchip(1))

    // VERIFY
    result.state should be(
      State(
        Set(
          PartialBot(BotId(1), None, Some(Microchip(1)), IndexedSeq())
        )
      )
    )
  }

  it should "add chip to an existing bot without chips" in {
    // SETUP
    val start = Processor.withInitialState(State(Set(
      Bot(1, Instruction(OutputId(1), OutputId(2)))
    )))

    // CALL
    val result = start.addChip(1, Microchip(1))

    // VERIFY
    result.state should be(
      State(
        Set(
          Bot(1, Instruction(OutputId(1), OutputId(2)), Microchip(1))
        )
      )
    )
  }

  it should "add chip to an existing bot with one chip" in {
    // SETUP
    val start = Processor.withInitialState(State(Set(
      Bot(1, Instruction(OutputId(0), OutputId(1)), Microchip(1)),
      Output(0),
      Output(1)
    )))

    // CALL
    val result = start.addChip(1, Microchip(2))

    // VERIFY
    result.states should be(
      IndexedSeq(
        State(Set(
          PartialBot(BotId(1), Some(Instruction(OutputId(0), OutputId(1))), Some(Microchip(1)), IndexedSeq()),
          Output(0),
          Output(1)
        )),
        State(Set(
          FullBot(BotId(1), Some(Instruction(OutputId(0), OutputId(1))), Microchip(1), Microchip(2), IndexedSeq()),
          Output(0),
          Output(1)
        )),
        State(Set(
          PartialBot(BotId(1), Some(Instruction(OutputId(0), OutputId(1))), None, IndexedSeq(Set(Microchip(1), Microchip(2)))),
          Output(0, IndexedSeq(Microchip(1))),
          Output(1, IndexedSeq(Microchip(2)))
        ))
      )
    )
  }

  it should "not add chip to an existing bot with two chips" in {
    // SETUP
    val start = Processor.withInitialState(State(Set(Bot(1, Instruction(OutputId(0), OutputId(1)), Microchip(1), Microchip(2)))))

    // CALL + VERIFY
    an [IllegalStateException] should be thrownBy start.addChip(1, Microchip(3))
  }

  it should "resolve the example" in {
    // SETUP
    /*
    value 5 goes to bot 2
    bot 2 gives low to bot 1 and high to bot 0
    value 3 goes to bot 1
    bot 1 gives low to output 1 and high to bot 0
    bot 0 gives low to output 2 and high to output 0
    value 2 goes to bot 2
    ----
    In the end,
      - output bin 0 contains a value-5 microchip,
      - output bin 1 contains a value-2 microchip,
      - and output bin 2 contains a value-3 microchip.

      In this configuration, bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.

    */

    val expectedState = State(Set(
      PartialBot(BotId(0), Some(Instruction(OutputId(2), OutputId(0))), None, IndexedSeq(Set(Microchip(5), Microchip(3)))),
      PartialBot(BotId(1), Some(Instruction(OutputId(1), BotId(0))), None, IndexedSeq(Set(Microchip(3), Microchip(2)))),
      PartialBot(BotId(2), Some(Instruction(BotId(1), BotId(0))), None, IndexedSeq(Set(Microchip(5), Microchip(2)))),
      Output(0, IndexedSeq(Microchip(5))),
      Output(1, IndexedSeq(Microchip(2))),
      Output(2, IndexedSeq(Microchip(3)))
    ))


    // CALL
    val processor = Processor()
      .addChip(2, Microchip(5))
      .addInstruction(2, Instruction(BotId(1), BotId(0)))
      .addChip(1, Microchip(3))
      .addInstruction(1, Instruction(OutputId(1), BotId(0)))
      .addInstruction(0, Instruction(OutputId(2), OutputId(0)))
      .addChip(2, Microchip(2)).resolve

    // VERIFY
    processor.state should be (expectedState)

    val bots = processor.states.flatMap(state => state.findBot(Microchip(5), Microchip(2) ) )
    val bots2 = (processor.state.bots.filter { case(id, bot) => bot.history.contains(Set(Microchip(5), Microchip(2))) }).values.toIndexedSeq

    bots.length should be (1)
    bots2.length should be (1)
    bots.head.id should be (BotId(2))
    bots2.head.id should be (BotId(2))

  }







}
