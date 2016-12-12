package net.hogerheijde.aoc2016.days.day2.model

sealed trait Key {
  def update(instruction: Instruction): Key
  def prettyString(): String
}

case object One extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => One
    case GoDown => Four
    case GoLeft => One
    case GoRight => Two
  }
  override def prettyString: String = "1"
}

case object Two extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Two
    case GoDown => Five
    case GoLeft => One
    case GoRight => Three
  }
  override def prettyString: String = "2"
}

case object Three extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Three
    case GoDown => Six
    case GoLeft => Two
    case GoRight => Three
  }
  override def prettyString: String = "3"
}

case object Four extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => One
    case GoDown => Seven
    case GoLeft => Four
    case GoRight => Five
  }
  override def prettyString: String = "4"
}

case object Five extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Two
    case GoDown => Eight
    case GoLeft => Four
    case GoRight => Six
  }
  override def prettyString: String = "5"
}

case object Six extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Three
    case GoDown => Nine
    case GoLeft => Five
    case GoRight => Six
  }
  override def prettyString: String = "6"
}

case object Seven extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Four
    case GoDown => Seven
    case GoLeft => Seven
    case GoRight => Eight
  }
  override def prettyString: String = "7"
}

case object Eight extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Five
    case GoDown => Eight
    case GoLeft => Seven
    case GoRight => Nine
  }
  override def prettyString: String = "8"
}

case object Nine extends Key {
  override def update(instruction: Instruction): Key = instruction match {
    case GoUp => Six
    case GoDown => Nine
    case GoLeft => Eight
    case GoRight => Nine
  }
  override def prettyString: String = "9"
}
