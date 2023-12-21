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

  val condition = (n: Int) => if(Number %2 == 0) n%2 == 0 else (n-1)%2 == 0

  val distances = step(startX, startY, Number)

  val count = distances.collect{
    case(k, s) if condition(k) => s.length
  }.sum

  println(count)

  def step(x: Int, y: Int, distance: Int): Map[Int, Seq[(Int, Int)]] = {
    (0 to distance).foldLeft(Map(0 -> Seq((x, y))), Set((x, y))) {
      case ((map, visited), dist) =>
        val allNodes = map(dist)
        val neighbors = allNodes.flatMap(n => getNeighbors(n._1, n._2, visited)).distinct
        (map + ((dist+1) -> neighbors), visited ++ neighbors.toSet)
    }._1
  }

  def getNeighbors(x:Int, y:Int, visited: Set[(Int, Int)]): Seq[(Int, Int)] =
    Seq(
      Option.when(x - 1 >= 0 && input(y)(x - 1) != '#' && !visited.contains((x - 1, y)))((x - 1, y)),
      Option.when(x + 1 < colNum && input(y)(x + 1) != '#' && !visited.contains((x + 1, y)))((x + 1, y)),
      Option.when(y - 1 >= 0 && input(y - 1)(x) != '#' && !visited.contains((x, y - 1)))((x, y - 1)),
      Option.when(y + 1 < rowNum && input(y + 1)(x) != '#' && !visited.contains((x, y + 1)))((x, y + 1))
    ).flatten
}

object Day21B extends App {
  final case class Plot(x: Long, y: Long, i: Int, j: Int)
  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines().toArray.map(_.toCharArray)
  val rowNum = input.length
  val colNum = input.head.length
  val start = (for {
    (row, y) <- input.zipWithIndex
    (elem, x) <- row.zipWithIndex if elem == 'S'
  } yield Plot(x, y, x, y)).head

  val Number = 5000

  val condition = (n: Int) => if (Number % 2 == 0) n % 2 == 0 else (n - 1) % 2 == 0

  val distances = step(start, Number)

  val count = distances.collect {
    case (k, s) if condition(k) => s.size
  }.sum

  println(count)

  def step(p: Plot, distance: Int): Map[Int, Set[Plot]] = {
    (0 to distance).foldLeft(Map(0 -> Set(p)), Set(p)) {
      case ((map, visited), dist) =>
        val allNodes = map(dist)
        val neighbors = allNodes.flatMap(p => getNeighbors(p, visited))
        (map + ((dist+1) -> neighbors), visited ++ neighbors)
    }._1
  }

  def getNeighbors(p: Plot, visited: Set[Plot]): Set[Plot] = {
    val i = p.i
    val j = p.j
    val left =
      if (i-1 < 0) Option.when(input(j)(colNum-1) != '#')(Plot(p.x-1, p.y, colNum-1, j))
      else Option.when(input(j)(i-1) != '#')(Plot(p.x-1, p.y, i-1, j))
    val right =
      if (i + 1 >=  colNum) Option.when(input(j)(0) != '#')(Plot(p.x+1, p.y, 0, j))
      else Option.when(input(j)(i + 1) != '#')(Plot(p.x+1, p.y, i + 1, j))
    val up =
      if (j - 1 < 0) Option.when(input(rowNum-1)(i) != '#')(Plot(p.x, p.y-1, i, rowNum-1))
      else Option.when(input(j-1)(i) != '#')(Plot(p.x, p.y-1, i, j-1))
    val down =
      if (j + 1 >= rowNum) Option.when(input(0)(i) != '#')(Plot(p.x, p.y+1, i, 0))
      else Option.when(input(j+1)(i) != '#')(Plot(p.x, p.y+1, i, j+1))
    Seq(left, right, up, down).flatten.filterNot(visited.contains).toSet
  }

}