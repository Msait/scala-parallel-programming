package reductions

import org.scalameter._

import scala.annotation.tailrec

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
    parenthesis(chars)
  }

  def parenthesis(tail: Array[Char]): Boolean = {
    def parenthesisCount(closed: Int, chars: Array[Char]): Int = {
      if (chars.isEmpty) closed
      else {
        if (closed < 0) return -1
        if(chars.head == '(')
          parenthesisCount(closed + 1, chars.tail)
        else if(chars.head == ')')
          parenthesisCount(closed - 1, chars.tail)
        else
          parenthesisCount(closed, chars.tail)
      }
    }

    if (tail.nonEmpty)
      parenthesisCount(0, tail) == 0
    else
      true
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  /*  FIXME:
      [Test Description] parBalance should work for nested parentheses and threshold 1
      [Observed Error] test has been aborted
      [Lost Points] 5
  * */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until)
        (arg1, arg2)
      else if (chars(idx) == '(') traverse(idx + 1, until, arg1 + 1, arg2)
      else if (chars(idx) == ')')
        if (arg1 > 0) traverse(idx + 1, until, arg1, arg2 + 1)
        else (arg1, arg2 + 1)
      else traverse(idx + 1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (threshold >= (until - from)) {
        traverse(from, until, 0 , 0)
      } else {
        val mid = from + (until - from) / 2
        val (left, right) = common.parallel(reduce(from, mid), reduce(mid, until))
        // check if left part of array starts with ')'
        if (left._1 == 0 && left._2 == 1)
          left
        else
        (left._1 + right._1, left._2 + right._2)
      }
    }

    val result = reduce(0, chars.length)
    // count open braces == count close braces
    result._1 == result._2
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
