package aoc23

object Day24A extends App {
  final case class Line(slopes: (Int, Int), point: (Long, Long))
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines()
  val Lower = 200000000000000L
  val Higher = 400000000000000L
  val Upper = Higher-Lower

  val lines = input.foldLeft(Seq.empty[Line]){
    case (acc, inp) =>
      inp match {
        case s"$x, $y, $_ @ $xv, $yv, $_" =>
          acc :+ Line((xv.toInt, yv.toInt), (x.toLong-Lower, y.toLong-Lower))
      }
  }

  val pairs = lines.combinations(2).map{
    case(pair) => (pair.head, pair(1)) match {
      case (a, b) if slope(a) == slope(b) => None
      case (a, b) =>
        intersect(a, b).flatMap {
          case (x, y) =>
            Option.when(inFuture(a, x, y) && inFuture(b, x, y))((x, y))
        }
    }
  }

  val inside = pairs.count(_.isDefined)

  println(inside)

  // y = slope1(x - x1) + y1
  // y = slope2(x - x2) + y2
  // slope1(x - x1) + y1 = slope2(x - x2) + y2
  // slope1x - slope1x1 + y1 = slope2x - slope2x2 + y2
  // (slope1 -slope2)x = slope1x1 - slope2x2 -y1 + y2
  // x = (slope1x1 - slope2x2 -y1 + y2)/(slope1 - slope2)
  def intersect(a: Line, b: Line): Option[(Float, Float)] = {
    val aSlope = slope(a)
    val bSlope = slope(b)
    val x = ((aSlope*a.point._1) - (bSlope*b.point._1) - a.point._2 + b.point._2)/(aSlope-bSlope)
    val y = aSlope*(x - a.point._1) + a.point._2
    Option.when(x >= 0 && y >= 0 && x <= Upper && y <= Upper)((x, y))
  }


  def inFuture(l: Line, x: Float, y: Float): Boolean = {
    val tx = if (x < l.point._1) l.slopes._1 < 0 else l.slopes._1 > 0
    val ty = if (y < l.point._2) l.slopes._2 < 0 else l.slopes._2 > 0
    tx && ty
  }

  def slope(l: Line): Float = l.slopes._2.toFloat / l.slopes._1

}

//object Day24B extends App {
//  final case class Data(x: Long, y: Long, z: Long)
//  final case class Line(slopes: Data, point: Data)
//  val input = scala.io.Source.fromResource("aoc23/test.txt").getLines()
//
//  val lines = input.foldLeft(Seq.empty[Line]){
//    case (acc, inp) =>
//      inp match {
//        case s"$x, $y, $z @ $xv, $yv, $zv" =>
//          acc :+ Line(Data(xv.toLong, yv.toLong, zv.toLong), Data(x.toLong, y.toLong, z.toLong))
//      }
//  }
//
//
//  def divisors(n: Long): Seq[Long] = {
//    var i = 1
//    var divisors = Seq.empty[Long]
//    while(i < (n / i)){
//      if (n % i == 0) {
//        divisors = divisors :+ i :+ (n/i)
//      }
//      i = i +1
//    }
//    divisors
//  }
//
//  def buildVelocities(f: Data => Long) = {
//    val velocMap = lines.foldLeft(Map.empty[Long, Seq[Long]]) {
//      case (acc, line) =>
//        val key = f(line.slopes)
//        val updated = acc.getOrElse(key, Seq.empty) :+ f(line.point)
//        acc + (key -> updated)
//    }
//    velocMap.foldLeft(Set.empty[Long]) {
//      case(opts, (v, ls)) =>
//        opts ++ ls.combinations(2).foldLeft(opts){
//          case (op, combs) =>
//            if (op.isEmpty) {
//              op ++ divisors(Math.abs(combs.head - combs(1))).flatMap { d =>
//                Set(d + v, -d + v)
//              }
//            } else {
//              op -- op.flatMap {
//                o =>
//                  Option.when(o == v || Math.abs(combs.head - combs(1)) % Math.abs(o - v) != 0)(
//                    o
//                )
//              }
//            }
//        }
//    }
//  }
//
//
//
//}
//
