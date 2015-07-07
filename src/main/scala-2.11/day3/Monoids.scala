package day3

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/7.
 */
object Monoids {
  // in the console do :k for retrieving the kind information of a type
  // such as :k Int and :k -v Int

  // Tagged type
  //
  // type Tagged[U] = { type Tag = U }
  // type @@[T, U] = T with Tagged[U]

  sealed trait KiloGram //defined trait KiloGram
  def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a) //KiloGram: [A](a: A)scalaz.@@[A,KiloGram]
  val mass = KiloGram(20.0) //mass: scalaz.@@[Double,KiloGram] = 20.0
  2 * Tag.unwrap(mass) //40.0
  2 * Tag.unwrap(mass) //error: wrong number of type parameters for method unwrap$mDc$sp: [T](a: Object{type Tag = T; type Self = Double})Double
                       //2 * Tag.unwrap(mass)
                       //^
  // ought to be ok in console, not tested
  //2 * scalaz.Tag.unsubst[Double, Id, KiloGram](mass) //40.0

  sealed trait JoulePerKiloGram //defined trait JoulePerKiloGram
  def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a) //JoulePerKiloGram: [A](a: A)scalaz.@@[A,JoulePerKiloGram]
  def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram =
    //JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unsubst[Double, Id, KiloGram](m)) //energyR: (m: scalaz.@@[Double,KiloGram])scalaz.@@[Double,JoulePerKiloGram]
    JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unwrap(m))
  energyR(mass) //scalaz.@@[Double,JoulePerKiloGram] = 1.79751035747363533E18




  // Semigroup introduce mappend:|+|
  // Monoid introduce zero:identity

  List(1, 2, 3) |+| List(4, 5, 6) //List[Int] = List(1, 2, 3, 4, 5, 6)
  "one" |+| "two" //String = onetwo
  Tags.Multiplication(10) |+| Monoid[Int @@ Tags.Multiplication].zero //scalaz.@@[Int,scalaz.Tags.Multiplication] = 10
  10 |+| Monoid[Int].zero //Int = 10

  Tags.Disjunction(true) |+| Tags.Disjunction(false) //scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true
  Monoid[Boolean @@ Tags.Disjunction].zero |+| Tags.Disjunction(true) //scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true
  Monoid[Boolean @@ Tags.Disjunction].zero |+| Monoid[Boolean @@ Tags.Disjunction].zero //scalaz.@@[Boolean,scalaz.Tags.Disjunction] = false
  Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(true) //scalaz.@@[Boolean,scalaz.Tags.Conjunction] = true
  Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(false) //scalaz.@@[Boolean,scalaz.Tags.Conjunction] = false

  (Ordering.LT: Ordering) |+| (Ordering.GT: Ordering) //scalaz.Ordering = LT
  (Ordering.GT: Ordering) |+| (Ordering.LT: Ordering) //scalaz.Ordering = GT
  Monoid[Ordering].zero |+| (Ordering.LT: Ordering) //scalaz.Ordering = LT
  Monoid[Ordering].zero |+| (Ordering.GT: Ordering) //scalaz.Ordering = GT

  def lengthCompare(lhs: String, rhs: String): Ordering =
    (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)

  lengthCompare("zen", "ants") //scalaz.Ordering = LT
  lengthCompare("zen", "ant") //scalaz.Ordering = GT

}