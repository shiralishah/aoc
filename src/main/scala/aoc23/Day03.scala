package aoc23


final case class Part(xStart: Int, xEnd: Int, y: Int, num: String) {
  def isValid(symbol: CharSymbol): Boolean = {
    val xVals = (this.xStart-1 to this.xEnd+1).toList
    val yVals = (this.y-1 to this.y+1).toList
    xVals.contains(symbol.x) && yVals.contains(symbol.y)
  }

  def isValid(symbols: Seq[Symbol]): Boolean = {
    symbols.exists(this.isValid)
  }
}

trait CharSymbol {
  def x: Int
  def y: Int
}

final case class Symbol(x: Int, y: Int, sym: String) extends CharSymbol

object Day03A extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq

  val (allParts, allSymbols) = input.zipWithIndex.foldLeft(Seq.empty[Part], Seq.empty[Symbol]) {
    case ((parts, symbols), (line, y)) =>
      val (p, partSet, symbolSet) = line.zipWithIndex.foldLeft(Option.empty[Part], Seq.empty[Part], Seq.empty[Symbol]) {
        case ((part, ps, ss), (char, x)) =>
          char match {
            case c if c.isDigit =>
              part match {
                case None => (Some(Part(x, x, y, c.toString)), ps, ss)
                case Some(prt) => (Some(prt.copy(xEnd = x, num = prt.num :+ c)), ps, ss)
              }
            case c if c == '.' => (None, part.fold(ps)(ps :+ _), ss)
            case c if !c.isDigit && c != '.' => (None, part.fold(ps)(ps :+ _), ss :+ Symbol(x, y, c.toString))
          }
      }
      (parts ++ p.fold(partSet)(partSet :+ _), symbols ++ symbolSet)
  }

  val validParts = allParts.filter(_.isValid(allSymbols))
  val total = validParts.map(_.num.toLong).sum
  println(total)
}
final case class Gear(x: Int, y:Int, parts: Seq[Part] = Seq.empty) extends CharSymbol {
  def mapParts(parts: Seq[Part]): Gear = {
    val adjacentParts = parts.filter(_.isValid(this))
    this.copy(parts = adjacentParts)
  }
  def calculateRatio: Long = parts.map(_.num.toLong).product
}
object Day03B extends App {
  val input = scala.io.Source.fromResource("aoc23/run.txt").getLines().toSeq

  val (allParts, allGears) = input.zipWithIndex.foldLeft(Seq.empty[Part], Seq.empty[Gear]) {
    case ((parts, gears), (line, y)) =>
      val (p, partSet, gearSet) = line.zipWithIndex.foldLeft(Option.empty[Part], Seq.empty[Part], Seq.empty[Gear]) {
        case ((part, ps, gs), (char, x)) =>
          char match {
            case c if c.isDigit =>
              part match {
                case None => (Some(Part(x, x, y, c.toString)), ps, gs)
                case Some(prt) => (Some(prt.copy(xEnd = x, num = prt.num :+ c)), ps, gs)
              }
            case c if c == '*' => (None, part.fold(ps)(ps :+ _), gs :+ Gear(x, y))
            case c if c == '.' || !c.isDigit => (None, part.fold(ps)(ps :+ _), gs)
          }
      }
      (parts ++ p.fold(partSet)(partSet :+ _), gears ++ gearSet)
  }
  val validGears = allGears.map(_.mapParts(allParts)).filter(_.parts.size == 2)
  val total = validGears.map(_.calculateRatio).sum
  println(total)
}