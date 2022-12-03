package aoc21

object Day06 {
  def calculateFishNum(DAYS: Int, input: Array[Int]): Long = {
    val initialFishes = input.groupMapReduce(identity)(_ => 1L)(_ + _)
    val (fishNum, _) = {
      (0 to (DAYS - 1)).foldLeft((input.length.toLong, initialFishes)) {
        case ((fishes, fishMap), day) =>
          fishMap.get(day) match {
            case None => (fishes, fishMap)
            case Some(num) =>
              val revivedFishes = fishMap.getOrElse(day + 7, 0L) + num
              val bornFishes = fishMap.getOrElse(day + 9, 0L) + num
              val updatedMap = if (day + 7 >= DAYS) {
                fishMap.removed(day)
              } else {
                fishMap.removed(day) + ((day + 7) -> revivedFishes) + ((day + 9) -> bornFishes)
              }
              (fishes + num, updatedMap)
          }
      }
    }
    fishNum
  }
}

object Day06A extends App {
  val DAYS = 80
  val input = scala.io.Source.fromResource("aoc21/day06.txt").getLines().toSeq.head.split(",").map(_.toInt)
  val fishes = Day06.calculateFishNum(DAYS, input)
  println(fishes)
}

object Day06B extends App {
  val DAYS = 256
  val input = scala.io.Source.fromResource("aoc21/day06.txt").getLines().toSeq.head.split(",").map(_.toInt)
  val fishes = Day06.calculateFishNum(DAYS, input)
  println(fishes)
}