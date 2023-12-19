package aoc23

import scala.annotation.tailrec

object Day19A extends App {

  final case class Part(scores: Map[String, Long]) {
    def apply(s: String): Long = scores(s)

    def value: Long = scores.values.sum
  }

  sealed trait Terminal

  object Terminal {
    case class Accepted(num: Long) extends Terminal

    case object Rejected extends Terminal
  }

  final case class Workflow(name: String, rules: Seq[Rule])

  final case class Rule(f: Part => Boolean, destination: String) {
    def apply(p: Part): Boolean = this.f(p)
  }

  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split("\n\n")
  val workflows = input(0).split("\n").foldLeft(Map.empty[String, Workflow]) {
    case (map, line) =>
      val workflow = line match {
        case s"$name{$conditions}" =>
          val rules = conditions.split(",").foldLeft(Seq.empty[Rule]) {
            case (rs, condition) =>
              val r = condition match {
                case s"$cat<$num:$dest" => Rule((p: Part) => p(cat) < num.toLong, dest)
                case s"$cat>$num:$dest" => Rule((p: Part) => p(cat) > num.toLong, dest)
                case s"$fc" => Rule((_: Part) => true, fc)
              }
              rs :+ r
          }
          Workflow(name, rules)
      }
      map + (workflow.name -> workflow)
  }
  val allParts = input(1).split("\n").foldLeft(Seq.empty[Part]) {
    case (ps, line) =>
      line match {
        case s"{$values}" =>
          val partMap = values.split(",").foldLeft(Map.empty[String, Long]) {
            case (map, item) =>
              item match {
                case s"$cat=$v" => map + (cat -> v.toLong)
              }
          }
          ps :+ Part(partMap)
      }
  }
  val acceptedVal = allParts.map(p => sort(p, "in")).collect {
    case accepted: Terminal.Accepted => accepted.num
  }.sum

  println(acceptedVal)

  @tailrec
  def sort(p: Part, workflowName: String): Terminal = {
    val workflow = workflows(workflowName)
    val next = workflow.rules.collectFirst {
      case rule if rule(p) => rule.destination
    }.get
    next match {
      case "A" => Terminal.Accepted(p.value)
      case "R" => Terminal.Rejected
      case other => sort(p, other)
    }
  }

}


object Day19B extends App {

  // inclusive
  final case class Range(low: Long, high: Long) {
    def count: Long = high-low + 1
  }

  final case class Part(scores: Map[String, Range]) {
    def split(rule: StepRule): (Part, Part) = {
      val range = scores(rule.category)
      if (rule.ifOver) {
        (
          this.copy(scores = this.scores + (rule.category -> range.copy(low = rule.threshold + 1))),
          this.copy(scores = this.scores + (rule.category -> range.copy(high = rule.threshold)))
        )
      } else {
        (
          this.copy(scores = this.scores + (rule.category -> range.copy(high = rule.threshold - 1))),
          this.copy(scores = this.scores + (rule.category -> range.copy(low = rule.threshold)))
        )
      }
    }

    def combinations: Long =
      scores.values.map(_.count).toSeq.product
  }

  final case class Workflow(name: String, rules: Seq[Rule])

  sealed trait Rule

  final case class StepRule(category: String, threshold: Long, destination: String, ifOver: Boolean) extends Rule
  final case class FinalRule(dest: String) extends Rule

  val input = scala.io.Source.fromResource("aoc23/run.txt").mkString.split("\n\n")
  val workflows = input(0).split("\n").foldLeft(Map.empty[String, Workflow]) {
    case (map, line) =>
      val workflow = line match {
        case s"$name{$conditions}" =>
          val rules = conditions.split(",").foldLeft(Seq.empty[Rule]) {
            case (rs, condition) =>
              val r: Rule = condition match {
                case s"$cat<$num:$dest" => StepRule(cat, num.toLong, dest, ifOver = false)
                case s"$cat>$num:$dest" => StepRule(cat, num.toLong, dest, ifOver = true)
                case s"$fc" => FinalRule(fc)
              }
              rs :+ r
          }
          Workflow(name, rules)
      }
      map + (workflow.name -> workflow)
  }

  val start = Part(Map("x" -> Range(1, 4000), "m" -> Range(1, 4000), "a" -> Range(1, 4000), "s" -> Range(1, 4000)))

  val combos = follow(start, "in", 0)

  println(combos)

  def follow(p: Part, workflowName: String, ruleNum: Int): Long = {
    workflowName match {
      case "A" => p.combinations
      case "R" => 0L
      case other =>
        val workflow = workflows(other)
        val rule = workflow.rules(ruleNum)
        rule match {
          case r: FinalRule => follow(p, r.dest, 0)
          case r: StepRule =>
            val (pass, fail) = p.split(r)
            follow(fail, workflowName, ruleNum+1) +
              follow(pass, r.destination, 0)
        }
    }
  }

}
