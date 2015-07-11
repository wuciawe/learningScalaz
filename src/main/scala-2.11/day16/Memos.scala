package day16

import scalaz._
import scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object Memos {
  def main(args: Array[String]) {
    def slowFib: Int => Int = {
      case 0 => 0
      case 1 => 1
      case n => slowFib(n - 2) + slowFib(n - 1)
    }

    def memoizedFib: Int => Int = Memo.mutableHashMapMemo {
      case 0 => 0
      case 1 => 1
      case n => memoizedFib(n - 2) + memoizedFib(n - 1)
    }

    // Under Memo object there are some default implementations of Memo
    // like Memo.mutableHashMapMemo[K, V], Memo.weakHashMapMemo[K, V],
    // and Memo.arrayMemo[V].

    // In general, we should be careful with any of these optimization
    // techniques. First the overall performance should be profiled to see
    // if it in fact would contribute to time savings, and second
    // space trade-off needs to be analyzed so it doesn’t grow endlessly.

    // An expression e is referentially transparent if every
    // occurrence e can be replaced with its value without affecting
    // the observable result of the program.

    // Memoization is one way of taking the advantage of
    // referential transparency.
  }
}
