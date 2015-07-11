package day8

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/9.
 */
object Monads2 {
  def main(args: Array[String]) {
    // In Scalaz Monad extends Applicative, so there��s no question that all monads
    // are functors. This means we can use map or <*> operator.

    //It turns out that any nested monadic value can be flattened and
    // that this is actually a property unique to monads. For this, the
    // join function exists.
    // In Scalaz join (and its symbolic alias μ) is a method introduced by Bind.

    (Some(9.some): Option[Option[Int]]).join //Option[Int] = Some(9)
    (Some(none): Option[Option[Int]]).join //Option[Int] = None
    List(List(1, 2, 3), List(4, 5, 6)).join //List[Int] = List(1, 2, 3, 4, 5, 6)
    9.right[String].right[String].join //scalaz.Unapply[scalaz.Bind,scalaz.\/[String,scalaz.\/[String,Int]]]{type M[X] = scalaz.\/[String,X]; type A = scalaz.\/[String,Int]}#M[Int] = \/-(9)
    "boom".left[Int].right[String].join //scalaz.Unapply[scalaz.Bind,scalaz.\/[String,scalaz.\/[String,Int]]]{type M[X] = scalaz.\/[String,X]; type A = scalaz.\/[String,Int]}#M[Int] = -\/(boom)

    // filterM, foldLeftM, foldRightM
    List(1, 2, 3) filterM { x => List(true, false) } //List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(1, 3), List(1), List(2, 3), List(2), List(3), List())
    Vector(1, 2, 3) filterM { x => Vector(true, false) } //scala.collection.immutable.Vector[Vector[Int]] = Vector(Vector(1, 2, 3), Vector(1, 2), Vector(1, 3), Vector(1), Vector(2, 3), Vector(2), Vector(3), Vector())

    def binSmalls(acc: Int, x: Int): Option[Int] = {
      if (x > 9) (none: Option[Int])
      else (acc + x).some
    }
    List(2, 8, 3, 1).foldLeftM(0) {binSmalls} //Option[Int] = Some(14)
    List(2, 11, 3, 1).foldLeftM(0) {binSmalls} //Option[Int] = None

    // RPN calculator
    def foldingFunction(list: List[Double], next: String): Option[List[Double]] = (list, next) match {
      case (x :: y :: ys, "*") => ((y * x) :: ys).point[Option]
      case (x :: y :: ys, "+") => ((y + x) :: ys).point[Option]
      case (x :: y :: ys, "-") => ((y - x) :: ys).point[Option]
      case (xs, numString) => numString.parseInt.toOption map { _ :: xs }
    }

    def solveRPN(s: String): Option[Double] = for {
      List(x) <- s.split(' ').toList.foldLeftM(Nil: List[Double]) {foldingFunction}
    } yield x

    solveRPN("10 4 3 + 2 * -")

    // Kleisli
    // In Scalaz Kleisli is the wrapper for function of type A => M[B]
    val f = Kleisli { (x: Int) => (x + 1).some } //scalaz.Kleisli[Option,Int,Int] = scalaz.KleisliFunctions$$anon$18@7da2734e
    val g = Kleisli { (x: Int) => (x * 100).some } //scalaz.Kleisli[Option,Int,Int] = scalaz.KleisliFunctions$$anon$18@49e07991
    // f <=< g means f compose g
    4.some >>= (f <=< g) //Option[Int] = Some(401)
    // f >=> g means f andThen g
    4.some >>= (f >=> g) //Optnion[Int] = Some(500)

    // Reader again, last time in day 6
    val addStuff: Reader[Int, Int] = for {
      a <- Reader { (_: Int) * 2 }
      b <- Reader { (_: Int) + 10 }
    } yield a + b
    addStuff(3) //scalaz.Id.Id[Int] = 19

    // Making monads
    case object Prob extends ProbInstances
    case class Prob[A] (list: List[(A, Double)]) extends ProbInstances
    trait ProbInstances {
      // functor monad
      implicit val probInstance = new Functor[Prob] with Monad[Prob] {
        def point[A](a: => A): Prob[A] = Prob((a, 1.0) :: Nil)
        def bind[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = flatten(map(fa)(f))
        override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] =
          Prob(fa.list map { case (x, p) => (f(x), p) })
      }
      implicit def probShow[A]: Show[Prob[A]] = Show.showA

      // flatten
      def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
        def multall(innerxs: Prob[B], p: Double) =
          innerxs.list map { case (x, r) => (x, p * r) }
        Prob((xs.list map { case (innerxs, p) => multall(innerxs, p) }).flatten)
      }
    }

    sealed trait Coin
    case object Heads extends Coin
    case object Tails extends Coin
    implicit val coinEqual: Equal[Coin] = Equal.equalA

    def coin: Prob[Coin] = Prob(Heads -> 0.5 :: Tails -> 0.5 :: Nil)
    def loadedCoin: Prob[Coin] = Prob(Heads -> 0.1 :: Tails -> 0.9 :: Nil)

    def flipThree: Prob[Boolean] = for {
      a <- coin
      b <- coin
      c <- loadedCoin
    } yield { List(a, b, c) all {_ === Tails} }

    flipThree //Prob[Boolean] = Prob(List((false,0.025), (false,0.225), (false,0.025), (false,0.225), (false,0.025), (false,0.225), (false,0.025), (true,0.225)))
  }

}
