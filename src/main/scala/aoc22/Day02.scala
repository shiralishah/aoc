package aoc22

sealed trait Outcome {
  val points: Int
}
object Outcome {
  case object WIN extends Outcome {
    override val points: Int = 6
  }
  case object LOSE extends Outcome {
    override val points: Int = 0
  }
  case object TIE extends Outcome {
    override val points: Int = 3
  }

  def unapply(str: String): Outcome =
    str match {
      case "X" => Outcome.LOSE
      case "Y" => Outcome.TIE
      case "Z" => Outcome.WIN
    }
}

sealed trait Hand {
  val points: Int
  val win: Hand
  val lose: Hand
  def getOutcome(opp: Hand): Outcome =
    this match {
      case opp.win  => Outcome.LOSE
      case opp.lose => Outcome.WIN
      case _        => Outcome.TIE
    }

  def getHand(outcome: Outcome): Hand =
    outcome match {
      case Outcome.LOSE => this.win
      case Outcome.WIN  => this.lose
      case _            => this
    }
}
object Hand {
  case object ROCK extends Hand {
    override val points = 1
    override val win    = Hand.SCISSORS
    override val lose   = Hand.PAPER

  }
  case object PAPER extends Hand {
    override val points = 2
    override val win    = Hand.ROCK
    override val lose   = Hand.SCISSORS

  }
  case object SCISSORS extends Hand {
    override val points = 3
    override val win    = Hand.PAPER
    override val lose   = Hand.ROCK

  }

  def unapply(str: String): Hand =
    str match {
      case "A" | "X" => Hand.ROCK
      case "B" | "Y" => Hand.PAPER
      case "C" | "Z" => Hand.SCISSORS
    }
}

object Day02A extends App {
  val input = scala.io.Source.fromResource("aoc22/day02.txt").getLines().toSeq.map(_.split(" "))
  val total = input.foldLeft(0) { case (points, round) =>
    val opp     = Hand.unapply(round(0))
    val mine    = Hand.unapply(round(1))
    val outcome = mine.getOutcome(opp)
    points + mine.points + outcome.points
  }
  println(total)
}

object Day02B extends App {
  val input = scala.io.Source.fromResource("aoc22/day02.txt").getLines().toSeq.map(_.split(" "))
  val total = input.foldLeft(0) { case (points, round) =>
    val opp     = Hand.unapply(round(0))
    val outcome = Outcome.unapply(round(1))
    val mine    = opp.getHand(outcome)
    points + mine.points + outcome.points
  }
  println(total)
}
