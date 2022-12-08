package aoc22

object Day08A extends App {
  val input  = scala.io.Source.fromResource("aoc22/day08.txt").getLines().map(_.toCharArray.map(_.asDigit)).toArray
  val width  = input.head.length
  val height = input.length
  val base   = 2 * (width - 1) + 2 * (height - 1)

  val visibility = (1 until (height - 1)).flatMap { y =>
    (1 until (width - 1)).map { x =>
      val h             = input(y)(x)
      val (left, right) = input(y).splitAt(x)
      val (up, down)    = input.transpose(identity)(x).splitAt(y)
      !(left.max >= h && right.tail.max >= h && up.max >= h && down.tail.max >= h)
    }
  }

  val visibleTrees = base + visibility.filter(identity).length

  println(visibleTrees)
}

object Day08B extends App {
  val input  = scala.io.Source.fromResource("aoc22/day08.txt").getLines().map(_.toCharArray.map(_.asDigit)).toArray
  val width  = input.head.length
  val height = input.length

  val maxScore = (0 until height).flatMap { y =>
    (0 until width).map { x =>
      val t             = input(y)(x)
      val (left, right) = input(y).zipWithIndex.splitAt(x)
      val (up, down)    = input.transpose(identity)(x).zipWithIndex.splitAt(y)
      val leftMax       = left.findLast(_._1 >= t).map(_._2).getOrElse(left.headOption.map(_._2).getOrElse(x))
      val rightMax      = right.tail.find(_._1 >= t).map(_._2).getOrElse(right.tail.lastOption.map(_._2).getOrElse(x))
      val upMax         = up.findLast(_._1 >= t).map(_._2).getOrElse(up.headOption.map(_._2).getOrElse(y))
      val downMax       = down.tail.find(_._1 >= t).map(_._2).getOrElse(down.tail.lastOption.map(_._2).getOrElse(y))
      (x - leftMax) * (rightMax - x) * (y - upMax) * (downMax - y)
    }
  }.max

  println(maxScore)
}
