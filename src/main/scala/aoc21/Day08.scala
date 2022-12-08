package aoc21

object Day08A extends App {
  val uniqueLengths = Seq(2, 3, 4, 7)
  val input         = scala.io.Source.fromResource("aoc21/day08.txt").getLines().toSeq
  val data = input.map { line =>
    line match {
      case s"$prefix | $suffix" => (prefix.split(' '), suffix.split(' '))
    }
  }
  val outputValues     = data.flatMap(_._2)
  val uniqueLengthVals = outputValues.groupBy(_.length).filter(x => uniqueLengths.contains(x._1))
  val uniqueDigitCount = uniqueLengthVals.flatMap(_._2).size
  println(uniqueDigitCount)
}

object Day08B extends App {
  val input = scala.io.Source.fromResource("aoc21/day08.txt").getLines().toSeq
  val data = input.map { line =>
    line match {
      case s"$prefix | $suffix" => (prefix.split(' '), suffix.split(' '))
    }
  }
  val outputs = data.map { d =>
    val groupedInp = d._1.groupBy(_.length)
    val wrapper = UniqueWrapper(
      groupedInp.get(2).map(_.head.toCharArray).getOrElse(Array.emptyCharArray),
      groupedInp.get(4).map(_.head.toCharArray).getOrElse(Array.emptyCharArray),
      groupedInp.get(3).map(_.head.toCharArray).getOrElse(Array.emptyCharArray),
      groupedInp.get(7).map(_.head.toCharArray).getOrElse(Array.emptyCharArray)
    )
    val outputStr = d._2.map(decipher(_, wrapper)).mkString
    Integer.parseInt(outputStr)
  }
  val total = outputs.sum
  println(total)

  def decipher(str: String, wrapper: UniqueWrapper): Int =
    str.length match {
      case 2 => 1
      case 3 => 7
      case 4 => 4
      case 7 => 8
      case 5 => wrapper.findFive(str)
      case 6 => wrapper.findSix(str)
    }
}

case class UniqueWrapper(one: Array[Char], four: Array[Char], seven: Array[Char], eight: Array[Char]) {
  def inOne(str: String): Boolean = one.nonEmpty && one.forall(str.toCharArray.contains)

  def findFive(str: String): Int = if (inOne(str)) {
    3
  } else if (four.filterNot(seven.contains).forall(str.toCharArray.contains)) {
    5
  } else {
    2
  }

  def findSix(str: String): Int =
    if (four.nonEmpty && four.forall(str.toCharArray.contains)) {
      9
    } else if (inOne(str)) {
      0
    } else {
      6
    }
}
