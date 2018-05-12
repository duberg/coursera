package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    (0 /: chars)({
      case (b ,'(') => b + 1
      case (b, ')') => if (b - 1 < 0) return false else b - 1
      case (b, _) => b
    }) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    * (if (zero? x) max (/ 1 x) )  --- ) (I told him (that it's not (yet) done). (But he wasn't listening)
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var acc1 = arg1
      var acc2 = arg2
      while (i < until) {
        chars(i) match {
          case '(' => acc1 += 1
          case ')' => if (arg1 > 0) acc1 -= 1 else acc2 += 1
          case _ =>
        }
        i += 1
      }
      (acc1, acc2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val ((r11, r12), (r21, r22)) = parallel(
          reduce(mid, until),
          reduce(from, mid)
        )
        if (r11 > r22) (r11 - r22 + r21) -> r12
        else r21 -> (r22 - r11 + r12)
      }
    }


    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
