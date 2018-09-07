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
  ) withWarmer (new Warmer.Default)

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

  /**
    * Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def iterate(index: Int, brackets: Int): Boolean =
      if (index == chars.length) brackets == 0
      else if (brackets < 0) false
      else if (chars(index) == '(') iterate(index + 1, brackets + 1)
      else if (chars(index) == ')') iterate(index + 1, brackets - 1)
      else iterate(index + 1, brackets)

    iterate(0, 0)
  }

  /**
    * Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(index: Int, until: Int, prefix: Int, suffix: Int): (Int, Int) =
      if (index >= until) (prefix, suffix)
      else if (chars(index) == '(') traverse(index + 1, until, prefix, suffix + 1)
      else if (chars(index) == ')')
        if (suffix > 0) traverse(index + 1, until, prefix, suffix - 1)
        else traverse(index + 1, until, prefix + 1, suffix)
      else traverse(index + 1, until, prefix, suffix)

    def reduce(from: Int, until: Int): (Int, Int) =
      if (until - from > threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle, until))
        val overlap = left._2 - right._1
        if (overlap < 0) (left._1 - overlap, right._2)
        else (left._1, overlap + right._2)
      }

    reduce(0, chars.length) == (0, 0)
  }
}
