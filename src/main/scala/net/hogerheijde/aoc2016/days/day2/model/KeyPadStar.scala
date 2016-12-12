package net.hogerheijde.aoc2016.days.day2.model

object KeyPadStar {
  case object One extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => One
      case GoDown => Three
      case GoLeft => One
      case GoRight => One
    }

    override def prettyString: String = "1"
  }

  case object Two extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Two
      case GoDown => Six
      case GoLeft => Two
      case GoRight => Three
    }

    override def prettyString: String = "2"
  }

  case object Three extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => One
      case GoDown => Seven
      case GoLeft => Two
      case GoRight => Four
    }

    override def prettyString: String = "3"
  }

  case object Four extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Four
      case GoDown => Eight
      case GoLeft => Three
      case GoRight => Four
    }

    override def prettyString: String = "4"
  }

  case object Five extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Five
      case GoDown => Five
      case GoLeft => Five
      case GoRight => Six
    }

    override def prettyString: String = "5"
  }

  case object Six extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Two
      case GoDown => A
      case GoLeft => Five
      case GoRight => Seven
    }

    override def prettyString: String = "6"
  }

  case object Seven extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Three
      case GoDown => B
      case GoLeft => Six
      case GoRight => Eight
    }

    override def prettyString: String = "7"
  }

  case object Eight extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Four
      case GoDown => C
      case GoLeft => Seven
      case GoRight => Nine
    }

    override def prettyString: String = "8"
  }

  case object Nine extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Nine
      case GoDown => Nine
      case GoLeft => Eight
      case GoRight => Nine
    }

    override def prettyString: String = "9"
  }

  case object A extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Six
      case GoDown => A
      case GoLeft => A
      case GoRight => B
    }

    override def prettyString: String = "A"
  }

  case object B extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Seven
      case GoDown => D
      case GoLeft => A
      case GoRight => C
    }

    override def prettyString: String = "B"
  }

  case object C extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => Eight
      case GoDown => C
      case GoLeft => B
      case GoRight => C
    }

    override def prettyString: String = "C"
  }

  case object D extends Key {
    override def update(instruction: Instruction): Key = instruction match {
      case GoUp => B
      case GoDown => D
      case GoLeft => D
      case GoRight => D
    }

    override def prettyString: String = "D"
  }
}
