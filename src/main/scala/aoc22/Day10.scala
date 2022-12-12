package aoc22

object Day10A extends App {
  val input        = scala.io.Source.fromResource("aoc22/day10.txt").getLines().toSeq
  val wantedCycles = Seq(20, 60, 100, 140, 180, 220)
  val (_, _, strengths) = input.foldLeft(1L, 1, Seq.empty[Long]) { case ((x, cycle, strengthList), inst) =>
    val (nextX, nextCycle) = inst match {
      case "noop"     => (x, cycle + 1)
      case s"addx $v" => (x + v.toLong, cycle + 2)
    }
    val strCycle = wantedCycles.intersect((cycle until nextCycle))
    val updatedList = if (strCycle.nonEmpty) {
      strengthList :+ (strCycle.head * x)
    } else strengthList
    (nextX, nextCycle, updatedList)
  }
  val strengthScore = strengths.sum
  println(strengthScore)
}

object Day10B extends App {
  val input           = scala.io.Source.fromResource("aoc22/day10.txt").getLines().toSeq
  val (width, height) = (40, 6)
  val (_, _, finalPic) = input.foldLeft(1L, 1, Array.fill(6)(Array.fill(40)('.'))) { case ((x, cycle, picture), inst) =>
    val i = (cycle - 1) % width
    val j = cycle / width

    val (nextX, nextCycle) = inst match {
      case "noop" =>
        val isVisible = ((x - 1) to (x + 1)).contains(i.toLong)
        if (isVisible) {
          picture(j)(i) = '#'
        }
        (x, cycle + 1)
      case s"addx $v" =>
        val isVisible = ((x - 1) to (x + 1)).intersect(Seq(i.toLong, (i + 1).toLong))
        if (isVisible.nonEmpty) {
          val update = isVisible.filter(k => k >= 0 && k <= (width - 1)).foreach(k => picture(j)(k.toInt) = '#')
        }
        (x + v.toLong, cycle + 2)
    }
    (nextX, nextCycle, picture)
  }
  finalPic.foreach(r => println(r.mkString))
}
