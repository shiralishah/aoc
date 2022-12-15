package aoc22

sealed trait Packet extends Ordered[Packet] {
  def :++(other: Packet): Packet
}

object Packet {
  def parse(str: String): Packet =
    str
      .zipWithIndex
      .foldLeft(Seq.empty[Int], Seq.empty[Packet], Option.empty[NumberPacket]) {
        case ((stack, packets, lastNum), (char, ind)) =>
          char match {
            case '[' => (ind +: stack, ListPacket(Seq.empty) +: packets, None)
            case ']' =>
              val newPackets    = lastNum.map(n => (packets.head :++ n) +: packets.drop(1)).getOrElse(packets)
              val latestPackets = newPackets.head
              val remPackets    = newPackets.drop(1)
              val prevPacket = remPackets.headOption match {
                case None    => latestPackets
                case Some(x) => x :++ latestPackets
              }
              val updated = prevPacket +: remPackets.drop(1)
              (stack.drop(1), updated, None)
            case ',' =>
              val newPackets = lastNum.map(n => (packets.head :++ n) +: packets.drop(1)).getOrElse(packets)
              (stack, newPackets, None)
            case _ =>
              val np        = NumberPacket(Integer.parseInt(char.toString))
              val newPacket = lastNum.map(n => n.join(np)).getOrElse(np)
              (stack, packets, Some(newPacket))
          }
      }
      ._2
      .head
}

case class ListPacket(values: Seq[Packet]) extends Packet {
  def :++(other: Packet): Packet =
    this.copy(values = values :+ other)

  override def compare(that: Packet): Int =
    that match {
      case p: NumberPacket                                          => this.compare(ListPacket(Seq(p)))
      case p: ListPacket if this.values.isEmpty && p.values.isEmpty => 0
      case p: ListPacket if this.values.isEmpty                     => -1
      case p: ListPacket if p.values.isEmpty                        => 1
      case p: ListPacket =>
        val head = this.values.head.compare(p.values.head)
        if (head == 0) {
          ListPacket(this.values.drop(1)).compare(ListPacket(p.values.drop(1)))
        } else head
    }
}

case class NumberPacket(value: Int) extends Packet {
  def :++(other: Packet): Packet =
    other match {
      case _: ListPacket => this
      case p: NumberPacket =>
        this.join(p)
    }

  def join(p: NumberPacket): NumberPacket = {
    val str = this.value.toString + p.value.toString
    this.copy(value = str.toInt)
  }

  override def compare(that: Packet): Int =
    that match {
      case p: NumberPacket =>
        if (this.value < p.value) -1
        else if (this.value > p.value) 1
        else 0
      case p: ListPacket => ListPacket(Seq(this)).compare(p)

    }
}

object Day13A extends App {
  val input =
    scala
      .io
      .Source
      .fromResource("aoc22/run.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n"))

  val (correct, _) = input.foldLeft(Seq.empty[Int], 1) { case ((indices, num), pair) =>
    val left  = Packet.parse(pair(0))
    val right = Packet.parse(pair(1))
    val updated =
      if (left <= right) indices :+ num
      else indices
    (updated, num + 1)
  }

  val correctSum = correct.sum
  println(correctSum)
}

object Day13B extends App {
  val input =
    scala
      .io
      .Source
      .fromResource("aoc22/run.txt")
      .mkString
      .replaceAll("\n\n", "\n")
      .split("\n")

  val div1 = ListPacket(Seq(ListPacket(Seq(NumberPacket(2)))))
  val div2 = ListPacket(Seq(ListPacket(Seq(NumberPacket(6)))))

  val allPackets: Array[Packet] = input.map(Packet.parse) :+ div1 :+ div2

  val sortedPackets = allPackets.sorted

  val div1Ind = sortedPackets.zipWithIndex.find(_._1 == div1).get._2
  val div2Ind = sortedPackets.zipWithIndex.find(_._1 == div2).get._2

  val product = (div1Ind + 1) * (div2Ind + 1)

  println(product)

}
