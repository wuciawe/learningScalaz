package day7

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object States {
  def main(args: Array[String]) {
    // A stateful computation is a function that takes some
    // state and returns a value along with some new state.
    State[List[Int], Int] { case x :: xs => (xs, x) }

    // A Stack with State
    type Stack = List[Int]
    val pop1 = State[Stack, Int] {
      case x :: xs => (xs, x)
    }
    def push1(a: Int) = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }
    def stackManip: State[Stack, Int] = for {
      _ <- push1(3)
      a <- pop1
      b <- pop1
    } yield(b)
    stackManip(List(5, 8, 2, 1)) //(Stack, Int) = (List(8, 2, 1),5)
    // Using State[List[Int], Int] {...} we were able to abstract out the "extract
    // state, and return value with a state" portion of the code. The powerful part is
    // the fact that we can monadically chain each operations using for syntax with-
    // out manually passing around the Stack values

    // State monad encapsulates functions that takes a state and returns a pair of a value and a state.
    // So get in the context of state simply means to retrieve the state into the value.
    // And put in this context means to put some value into the state.

    def stackyStack: State[Stack, Unit] = for {
      stackNow <- get
      r <- if (stackNow === List(1, 2, 3)) put(List(8, 3, 1))
      else put(List(9, 2, 1))
    } yield r
    stackyStack(List(1, 2, 3)) //(Stack, Unit) = (List(8, 3, 1),())
    val pop: State[Stack, Int] = for {
        s <- get[Stack]
        val (x :: xs) = s
        _ <- put(xs)
      } yield x
    def push(x: Int): State[Stack, Unit] = for {
      xs <- get[Stack]
      r <- put(x :: xs)
    } yield r
  }
}
