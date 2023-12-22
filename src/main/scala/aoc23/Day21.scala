package aoc23

object Day21A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowNum = input.length
  val colNum = input.head.length
  val (startX, startY) = (for {
    (row, y) <- input.zipWithIndex
    (elem, x) <- row.zipWithIndex if elem == 'S'
  } yield (x, y)).head

  val Number = 64

  val condition = (n: Int) => if (Number % 2 == 0) n % 2 == 0 else (n - 1) % 2 == 0

  val distances = step(startX, startY, Number)

  val count = distances.collect {
    case (k, s) if condition(k) => s.length
  }.sum

  println(count)

  def step(x: Int, y: Int, distance: Int): Map[Int, Seq[(Int, Int)]] = {
    (0 to distance).foldLeft(Map(0 -> Seq((x, y))), Set((x, y))) {
      case ((map, visited), dist) =>
        val allNodes = map(dist)
        val neighbors = allNodes.flatMap(n => getNeighbors(n._1, n._2, visited)).distinct
        (map + ((dist + 1) -> neighbors), visited ++ neighbors.toSet)
    }._1
  }

  def getNeighbors(x: Int, y: Int, visited: Set[(Int, Int)]): Seq[(Int, Int)] =
    Seq(
      Option.when(x - 1 >= 0 && input(y)(x - 1) != '#' && !visited.contains((x - 1, y)))((x - 1, y)),
      Option.when(x + 1 < colNum && input(y)(x + 1) != '#' && !visited.contains((x + 1, y)))((x + 1, y)),
      Option.when(y - 1 >= 0 && input(y - 1)(x) != '#' && !visited.contains((x, y - 1)))((x, y - 1)),
      Option.when(y + 1 < rowNum && input(y + 1)(x) != '#' && !visited.contains((x, y + 1)))((x, y + 1))
    ).flatten
}

object Day21B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toArray.map(_.toCharArray)
  val rowNum = input.length
  val colNum = input.head.length
  val (startX, startY) = (for {
    (row, y) <- input.zipWithIndex
    (elem, x) <- row.zipWithIndex if elem == 'S'
  } yield (x, y)).head

  val Number = 26501365

  val isEven = (n: Int) => n % 2 == 0

  val toEdge = rowNum/2
  val n = ((Number-toEdge)/colNum).toLong

  val distances = step(startX, startY, rowNum)

  val (even, odd) = distances.partition(d => isEven(d._1))
  val evenCount = even.values.flatten.toSet.size
  val oddCount = odd.values.flatten.toSet.size

  val corners = distances.filter(d => d._1 > toEdge)
  val (evenCorner, oddCorner) = corners.partition(d => isEven(d._1))
  val evenCornerCount = evenCorner.values.flatten.toSet.size
  val oddCornerCount = oddCorner.values.flatten.toSet.size

  val total: Long = ((n+1)*(n+1))*oddCount + (n*n)*evenCount - (n+1)*oddCornerCount + n*evenCornerCount - n

  println(total)

  def step(x: Int, y: Int, distance: Int): Map[Int, Seq[(Int, Int)]] = {
    (0 until  distance).foldLeft(Map(0 -> Seq((x, y))), Set((x, y))) {
      case ((map, visited), dist) =>
        val allNodes = map(dist)
        val neighbors = allNodes.flatMap(n => getNeighbors(n._1, n._2, visited)).distinct
        (map + ((dist + 1) -> neighbors), visited ++ neighbors.toSet)
    }._1
  }


  def getNeighbors(x: Int, y: Int, visited: Set[(Int, Int)]): Seq[(Int, Int)] =
    Seq(
      Option.when(x - 1 >= 0 && input(y)(x - 1) != '#' && !visited.contains((x - 1, y)))((x - 1, y)),
      Option.when(x + 1 < colNum && input(y)(x + 1) != '#' && !visited.contains((x + 1, y)))((x + 1, y)),
      Option.when(y - 1 >= 0 && input(y - 1)(x) != '#' && !visited.contains((x, y - 1)))((x, y - 1)),
      Option.when(y + 1 < rowNum && input(y + 1)(x) != '#' && !visited.contains((x, y + 1)))((x, y + 1))
    ).flatten


}
