package aoc22

object Day03A extends App {
  val input = scala.io.Source.fromResource("aoc22/day03.txt").getLines().toSeq
  val rucksacks = input.map { rucksack =>
    rucksack.splitAt(rucksack.length / 2)
  }
  val commons    = rucksacks.map(rucksack => rucksack._1.intersect(rucksack._2).head)
  val priorities = commons.map(item => if (item.isLower) item.toInt - 96 else item.toInt - 38).sum
  println(priorities)
}

object Day03B extends App {
  val input = scala.io.Source.fromResource("aoc22/day03.txt").getLines().sliding(3, 3).toSeq
  val commons = input.map { rucksackGroup =>
    rucksackGroup(0).intersect(rucksackGroup(1)).intersect(rucksackGroup(2)).head
  }
  val priorities = commons.map(item => if (item.isLower) item.toInt - 96 else item.toInt - 38).sum
  println(priorities)
}
