package day7

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object EithersYValidations {
  def main(args: Array[String]) {
    // Scalaz¡¯s \/ assumes that you¡¯d mostly want right projection.
    1.right[String] //scalaz.\/[String,Int] = \/-(1)
    "error".left[Int] //scalaz.\/[String,Int] = -\/(error)

    "boom".left[Int] >>= { x => (x + 1).right } //scalaz.Unapply[scalaz.Bind,scalaz.\/[String,Int]]{type M[X] = scalaz.\/[String,X]; type A = Int}#M[Int] = -\/(boom)
    for {
      e1 <- "event 1 ok".right
      e2 <- "event 2 failed!".left[String]
      e3 <- "event 3 failed!".left[String]
    } yield (e1 |+| e2 |+| e3) //scalaz.\/[String,String] = -\/(event 2 failed!)

    // | is the alias to getOrElse
    "event 1 ok".right | "something bad" //String = event 1 ok
    // unary_~ is the alias to swap
    ~"event 2 failed!".left[String] | "something good" //String = event 2 failed!
    // map function
    "event 1 ok".right map {_ + "!"} //scalaz.\/[Nothing,String] = \/-(event 1 ok!)
    // ||| is the alias to orElse
    "event 1 failed!".left ||| "retry event 1 ok".right //scalaz.\/[String,String] = \/-(retry event 1 ok)

    // Validations
    // Validation and \/ can even be converted back and forth using validation method and disjunction method.
    "event 1 ok".success[String] //scalaz.Validation[String,String] = Success(event 1 ok)
    "event 1 failed!".failure[String] //scalaz.Validation[String,String] = Failure(event 1 failed!)

    // Validation is not a Monad, but an Applicative functor.
    ("event 1 ok".success[String] |@| "event 2 failed!".failure[String] |@| "event 3 failed!".failure[String]) {_ + _ + _} //scalaz.Unapply[scalaz.Apply,scalaz.Validation[String,String]]{type M[X] = scalaz.Validation[String,X]; type A = String}#M[String] = Failure(event 2 failed!event 3 failed!)
    // Unlike \/ monad which cut the calculation short, Validation keeps going and reports back all failures.

    // NonEmptyList
    1.wrapNel //scalaz.NonEmptyList[Int] = NonEmptyList(1)
    "event 1 ok".successNel[String] //scalaz.ValidationNEL[String,String] = Success(event 1 ok)
    "event 1 failed!".failureNel[String] //scalaz.ValidationNEL[String,String] = Failure(NonEmptyList(event 1 failed!))
    ("event 1 ok".successNel[String] |@| "event 2 failed!".failureNel[String] |@| "event 3 failed!".failureNel[String]) {_ + _ + _} //scalaz.Unapply[scalaz.Apply,scalaz.ValidationNEL[String,String]]{type M[X] = scalaz.ValidationNEL[String,X]; type A = String}#M[String] = Failure(NonEmptyList(event 2 failed!, event 3 failed!))
  }
}
