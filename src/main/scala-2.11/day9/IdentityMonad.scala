package day9

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object IdentityMonad {

//  trait IdOps[A] extends Ops[A] {
//    /**Returns `self` if it is non-null, otherwise returns `d`. */
//    final def ??(d: => A)(implicit ev: Null <:< A): A =
//      if (self == null) d else self
//    /**Applies `self` to the provided function */
//    final def |>[B](f: A => B): B = f(self)
//    final def squared: (A, A) = (self, self)
//    def left[B]: (A \/ B) = \/.left(self)
//    def right[B]: (B \/ A) = \/.right(self)
//    final def wrapNel: NonEmptyList[A] = NonEmptyList(self)
//    /** @return the result of pf(value) if defined, otherwise the the Zero element of type B. */
//    def matchOrZero[B: Monoid](pf: PartialFunction[A, B]): B = ...
//    /** Repeatedly apply `f`, seeded with `self`, checking after each iteration whether the predicate `p` holds. */
//    final def doWhile(f: A => A, p: A => Boolean): A = ...
//    /** Repeatedly apply `f`, seeded with `self`, checking before each iteration whether the predicate `p` holds. */
//    final def whileDo(f: A => A, p: A => Boolean): A = ...
//    /** If the provided partial function is defined for `self` run this,
//     * otherwise lift `self` into `F` with the provided [[scalaz.Pointed]]. */
//    def visit[F[_] : Pointed](p: PartialFunction[A, F[A]]): F[A] = ...
//  }

  def main(args: Array[String]) {
    1 + 2 + 3 |> {_.point[List]} //List[Int] = List(6)
    1 + 2 + 3 |> {_ * 6} //Int = 36
    1 visit { case x@(2|3) => List(x * 2) } //List[Int] = List(1)
    2 visit { case x@(2|3) => List(x * 2) } //List[Int] = List(4)
  }
}