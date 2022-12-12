package aoc22

sealed trait Item {
  def addOperation(x: Option[Long]): Item
  def multOperation(x: Option[Long]): Item
  def modOperation(x: Long, num: Int): Boolean
}

case class ReliefItem(value: Long) extends Item {
  override def addOperation(x: Option[Long]): Item =
    this.copy(value = x.map(v => value + v).getOrElse(value + value)).addRelief()

  override def multOperation(x: Option[Long]): Item =
    this.copy(value = x.map(v => value * v).getOrElse(value * value)).addRelief()

  override def modOperation(x: Long, num: Int): Boolean =
    value % x == 0

  def addRelief(): Item = this.copy(value = value / 3)
}
case class StressItem(divisors: Seq[Long], values: Seq[Long]) extends Item {

  override def addOperation(x: Option[Long]): Item =
    this.copy(values = values.zipWithIndex.map { case (v, i) =>
      (v + x.getOrElse(v)) % divisors(i)
    })

  override def multOperation(x: Option[Long]): Item =
    this.copy(values = values.zipWithIndex.map { case (v, i) =>
      (v * x.getOrElse(v)) % divisors(i)
    })

  override def modOperation(x: Long, num: Int): Boolean =
    values(num) == 0L
}

case class Modulus(v: Long, n: Int) {
  def execute(item: Item): Boolean =
    item.modOperation(v, n)
}

sealed trait Operation {
  def execute(item: Item): Item
}
object Operation {
  def read(op: String, v: String): Operation = op match {
    case "+" => Addition(v.toLongOption)
    case "*" => Multiplication(v.toLongOption)
  }
}
case class Addition(v: Option[Long]) extends Operation {
  override def execute(item: Item): Item = item.addOperation(v)
}
case class Multiplication(v: Option[Long]) extends Operation {
  override def execute(item: Item): Item = item.multOperation(v)
}

case class MonkeyTest(check: Modulus, t: Int, f: Int) {
  def run(x: Item): Int =
    if (check.execute(x)) t else f
}

case class Monkey(num: Int, items: Seq[Item], op: Operation, test: MonkeyTest, inspections: Long) {
  def throwItems(): (Seq[(Item, Int)], Monkey) = {
    val toThrow = items.map { item =>
      val priority   = op.execute(item)
      val nextMonkey = test.run(priority)
      (priority, nextMonkey)
    }
    (toThrow, this.copy(items = Seq.empty, inspections = inspections + toThrow.length))
  }

  def catchItem(item: Item): Monkey = this.copy(items = items :+ item)
}

object Monkey {
  def empty: Monkey = Monkey(-1, Seq.empty, Addition(None), MonkeyTest(Modulus(0, -1), -1, -1), 0L)
}

object Day11A extends App {
  val input = scala.io.Source.fromResource("aoc22/day11.txt").mkString.split("\n\n")
  val monkeyStart =
    input.foldLeft(Map.empty[Int, Monkey]) { case (monkeyMap, inp) =>
      val monk = inp.split("\n").foldLeft(Monkey.empty) { case (m, line) =>
        line.stripLeading() match {
          case s"Monkey $monkeyNum:" => m.copy(num = monkeyNum.toInt)
          case s"Starting items: $itemList" =>
            val items = itemList.split(", ").map(_.toLong)
            m.copy(items = items.map(ReliefItem))
          case s"Operation: new = old $op $v" => m.copy(op = Operation.read(op, v))
          case s"Test: divisible by $div" =>
            m.copy(test = m.test.copy(check = Modulus(div.toLong, m.num)))
          case s"If true: throw to monkey $tMonk" =>
            m.copy(test = m.test.copy(t = tMonk.toInt))
          case s"If false: throw to monkey $fMonk" =>
            m.copy(test = m.test.copy(f = fMonk.toInt))
        }
      }
      monkeyMap.updated(monk.num, monk)
    }

  val finalMonkeys =
    (0 until 20).foldLeft(monkeyStart) { case (monkeys, _) =>
      monkeys.keys.toSeq.sorted.foldLeft(monkeys) { case (map, num) =>
        val monk               = map.getOrElse(num, Monkey.empty)
        val (toThrow, newMonk) = monk.throwItems()
        val afterThrows = toThrow.foldLeft(map) { case (m, (item, dest)) =>
          val destMonkey = m.getOrElse(dest, Monkey.empty)
          m.updated(dest, destMonkey.catchItem(item))
        }
        afterThrows.updated(num, newMonk)
      }
    }

  val sortedMonkeys = finalMonkeys.values.toSeq.sortBy(_.inspections).reverse
  val score         = sortedMonkeys(0).inspections * sortedMonkeys(1).inspections
  println(score)
}

object Day11B extends App {
  val input = scala.io.Source.fromResource("aoc22/day11.txt").mkString.split("\n\n")
  val tempMonkey =
    input.foldLeft(Map.empty[Int, Monkey]) { case (monkeyMap, inp) =>
      val monk = inp.split("\n").foldLeft(Monkey.empty) { case (m, line) =>
        line.stripLeading() match {
          case s"Monkey $monkeyNum:" => m.copy(num = monkeyNum.toInt)
          case s"Starting items: $itemList" =>
            val items = itemList.split(", ").map(_.toLong)
            m.copy(items = items.map(ReliefItem))
          case s"Operation: new = old $op $v" => m.copy(op = Operation.read(op, v))
          case s"Test: divisible by $div" =>
            m.copy(test = m.test.copy(check = Modulus(div.toLong, m.num)))
          case s"If true: throw to monkey $tMonk" =>
            m.copy(test = m.test.copy(t = tMonk.toInt))
          case s"If false: throw to monkey $fMonk" =>
            m.copy(test = m.test.copy(f = fMonk.toInt))
        }
      }
      monkeyMap.updated(monk.num, monk)
    }

  val divisors = tempMonkey.values.toSeq.sortBy(_.num).map(_.test.check.v)
  val monkeyStart = tempMonkey.foldLeft(tempMonkey) { case (newMonkeys, (n, m)) =>
    newMonkeys.updated(n, convertItem(m, divisors))
  }

  val finalMonkeys =
    (0 until 10000).foldLeft(monkeyStart) { case (monkeys, round) =>
      monkeys.keys.toSeq.sorted.foldLeft(monkeys) { case (map, num) =>
        val monk               = map.getOrElse(num, Monkey.empty)
        val (toThrow, newMonk) = monk.throwItems()
        val afterThrows = toThrow.foldLeft(map) { case (m, (item, dest)) =>
          val destMonkey = m.getOrElse(dest, Monkey.empty)
          m.updated(dest, destMonkey.catchItem(item))
        }
        afterThrows.updated(num, newMonk)
      }
    }

  val sortedMonkeys = finalMonkeys.values.toSeq.sortBy(_.inspections).reverse
  val score         = sortedMonkeys(0).inspections * sortedMonkeys(1).inspections
  println(score)

  def convertItem(m: Monkey, divisors: Seq[Long]): Monkey = {
    val updatedItems = m.items.map { item =>
      item match {
        case other: StressItem => other
        case x: ReliefItem =>
          val v      = x.value
          val values = divisors.map(d => v % d)
          StressItem(divisors, values)
      }
    }
    m.copy(items = updatedItems)
  }
}
