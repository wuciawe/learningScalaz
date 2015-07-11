package day15

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object Arrows {
  def main(args: Array[String]) {
    // An arrow is the term used in category theory as
    // an abstract notion of thing that behaves like a function.
    // The type signature of Arrow[=>:[_, _]] looks a bit odd,
    // but this is no different than saying Arrow[M[_, _]].
    // The neat things about type constructor that takes
    // two type parameters is that we can write =>:[A, B] as A =>: B.

    // Category and Compose
    val f = (_:Int) + 1 //Int => Int = <function1>
    val g = (_:Int) * 100 //Int => Int = <function1>
    (f >>> g)(2) //Int = 300
    (f <<< g)(2) //Int = 201

    // (***) combines two arrows into a new arrow by running the
    // two arrows on a pair of values (one arrow on the first item
    // of the pair and one arrow on the second item of the pair).
    (f *** g)(1, 2) //(Int, Int) = (2,200)
    // (&&&) combines two arrows into a new arrow by
    // running the two arrows on the same value.
    (f &&& g)(2) //(Int, Int) = (3,200)

    // and skip the unapply and parallel composition
  }
}
