package aoc23

final case class Hands(single: Seq[Hand] = Seq.empty,
                       pair: Seq[Hand] = Seq.empty,
                       double: Seq[Hand]= Seq.empty,
                       three: Seq[Hand] = Seq.empty,
                       full: Seq[Hand]= Seq.empty,
                       four: Seq[Hand] = Seq.empty,
                       five: Seq[Hand] = Seq.empty
                      ) {
  def rankOrder(ordering: Ordering[Hand]): Seq[Hand] =
    (this.five.sorted(ordering) :++
      this.four.sorted(ordering) :++
      this.full.sorted(ordering) :++
      this.three.sorted(ordering) :++
      this.double.sorted(ordering) :++
      this.pair.sorted(ordering) :++
      this.single.sorted(ordering)).reverse
}

final case class Hand(cards: String, bid: Long)

object CamelPokerOrderingA extends Ordering[Hand] {
  private val charOrder: Map[Char, Int] = Map(
    'A' -> 0, 'K' -> 1, 'Q' -> 2, 'J' -> 3, 'T' -> 4, '9' -> 5,
    '8' -> 6, '7' -> 7, '6' -> 8, '5' -> 9, '4' -> 10, '3' -> 11, '2' -> 12
  )

  def compare(x: Hand, y: Hand): Int =
    Iterator.range(0, x.cards.length)
      .map(i => charOrder(x.cards.charAt(i)) - charOrder(y.cards.charAt(i)))
      .find(_ != 0)
      .getOrElse(0)
}

object Day07A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val collectedHands = input.foldLeft(Hands()){
    case(hands, line) =>
      val splits = line.split(' ')
      val (hand, bid) = (splits(0), splits(1).toLong)
      val set = hand.toSet
      val counts = set.toSeq.map(c => hand.count(_ == c))
      val updated = set.size match {
        // 5 of a kind
        case 1 => hands.copy(five = hands.five :+ Hand(hand, bid))
        // single pair
        case 4 => hands.copy(pair = hands.pair :+ Hand(hand, bid))
        // single hand
        case 5 => hands.copy(single = hands.single :+ Hand(hand, bid))
        // three of a kind
        case 3 if counts.contains(3) => hands.copy(three = hands.three :+ Hand(hand, bid))
        // two pair
        case 3 => hands.copy(double = hands.double :+ Hand(hand, bid))
        // four of a kind
        case 2 if counts.contains(4) => hands.copy(four = hands.four :+ Hand(hand, bid))
        // full house
        case 2 => hands.copy(full = hands.full :+ Hand(hand, bid))
      }
      updated
  }
  val handsByRank = collectedHands.rankOrder(CamelPokerOrderingA).zipWithIndex
  val finalSum = handsByRank.foldLeft(0L) {
    case (total, (h, r)) =>
      total + h.bid*(r+1)
  }

  println(finalSum)
}

object CamelPokerOrderingB extends Ordering[Hand] {
  private val charOrder: Map[Char, Int] = Map(
    'A' -> 0, 'K' -> 1, 'Q' -> 2, 'T' -> 3, '9' -> 4, '8' -> 5,
    '7' -> 6, '6' -> 7, '5' -> 8, '4' -> 9, '3' -> 10, '2' -> 11, 'J' -> 12
  )

  def compare(x: Hand, y: Hand): Int =
    Iterator.range(0, x.cards.length)
      .map(i => charOrder(x.cards.charAt(i)) - charOrder(y.cards.charAt(i)))
      .find(_ != 0)
      .getOrElse(0)
}

object Day07B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq
  val collectedHands = input.foldLeft(Hands()){
    case(hands, line) =>
      val splits = line.split(' ')
      val (hand, bid) = (splits(0), splits(1).toLong)
      val (jokers, other) = hand.partition(_ == 'J')
      val set = other.toSet
      val counts = set.toSeq.map(c => other.count(_ == c))
      val maxCount = Option.when(counts.nonEmpty)(counts.max).getOrElse(0) + jokers.length
      val updated = maxCount match {
        // 5 of a kind
        case 5 => hands.copy(five = hands.five :+ Hand(hand, bid))
        // four of a kind
        case 4 => hands.copy(four = hands.four :+ Hand(hand, bid))
        // three of a kind
        case 3 if set.size == 3 => hands.copy(three = hands.three :+ Hand(hand, bid))
        // full house
        case 3 => hands.copy(full = hands.full :+ Hand(hand, bid))
        // pair
        case 2 if set.size == 4 => hands.copy(pair = hands.pair :+ Hand(hand, bid))
        // double pair
        case 2 => hands.copy(double = hands.double :+ Hand(hand, bid))
        // single hand
        case 1 => hands.copy(single = hands.single :+ Hand(hand, bid))
      }
      updated
  }
  val handsByRank = collectedHands.rankOrder(CamelPokerOrderingB).zipWithIndex
  val finalSum = handsByRank.foldLeft(0L) {
    case (total, (h, r)) =>
      total + h.bid*(r+1)
  }

  println(finalSum)
}

