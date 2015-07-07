package day2

import scalaz._
import Scalaz._
import scalaz.syntax.Ops

/**
 * Created by haha on 2015/6/19.
 */

trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait FunctorOps[F[_], A] extends Ops[F[A]] {
  implicit def F: Functor[F]
  import scalaz.Leibniz.===
  final def map[B](f: A => B): F[B] = F.map(self)(f)
}

trait Applicative[F[_]] extends Apply[F] { self =>
  def point[A](a: => A): F[A]
  /** alias for `point` */
  def pure[A](a: => A): F[A] = point(a)
}

object Functors {

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil => (Nil: List[A]).point[F]
    case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
  }

  def main(args: Array[String]) {
    List(1,2,3) map { _ + 1 } println
    val funcMap = ((x: Int) => x + 1) map { _ * 7 }
    funcMap(3) println

    List(1,2,3) map {3 *}
    List(1,2,3) map {_ * 3} // underscore _ is necessary

    val f1 = Functor[List].lift {(_: Int) * 2}
    f1(List(1,2,3))

    List(1, 2, 3) >| "x" // List[String] = List(x, x, x)
    List(1, 2, 3) as "x" // List[String] = List(x, x, x)
    List(1, 2, 3).fpair //List[(Int, Int)] = List((1,1), (2,2), (3,3))
    List(1, 2, 3).strengthL("x") //List[(String, Int)] = List((x,1), (x,2), (x,3))
    List(1, 2, 3).strengthR("x") //List[(Int, String)] = List((1,x), (2,x), (3,x))
    List(1, 2, 3).void //List[Unit] = List((), (), ())

    val f2 = List(1, 2, 3, 4) map {(_: Int) * (_:Int)}.curried //List[Int => Int] = List(<function1>, <function1>, <function1>, <function1>)
    f2 map {_(9)} //apply 9 with the function objects;List[Int] = List(9, 18, 27, 36)

    1.point[Option] //Option[Int] = Some(1)
    1.point[Option] map {_ + 2} //Option[Int] = Some(3)

    9.some <*> {(_: Int) + 3}.some //Option[Int] = Some(12)
    1.some <* 2.some //Option[Int] = Some(1)
    none <* 2.some //Option[Nothing] = None
    1.some *> 2.some //Option[Int] = Some(2)
    none *> 2.some //Option[Int] = None

    ^(3.some, 5.some) {_ + _} //Option[Int] = Some(8)
    ^(3.some, none[Int]) {_ + _} //Option[Int] = None
    (3.some |@| 5.some) {_ + _} //Option[Int] = Some(8)

    List(1, 2, 3) <*> List((_: Int) * 0, (_: Int) + 100, (x: Int) => x * x) //List[Int] = List(0, 0, 0, 101, 102, 103, 1, 4, 9)
    List(3, 4) <*> { List(1, 2) <*> List({(_: Int) + (_: Int)}.curried, {(_: Int) * (_: Int)}.curried) } //List[Int] = List(4, 5, 5, 6, 3, 4, 6, 8)
    (List("ha", "heh", "hmm") |@| List("?", "!", ".")) {_ + _} //List[String] = List(ha?, ha!, ha., heh?, heh!, heh., hmm?, hmm!, hmm.)

    // zip of lists
    val r1 = streamZipApplicative.ap(Tags.Zip(Stream(1, 2))) (Tags.Zip(Stream({(_: Int) + 3}, {(_: Int) * 2}))) //scala.collection.immutable.Stream[Int] with Object{type Tag = scalaz.Tags.Zip} = Stream(4, ?)
    //?? eh!
    //r1.toList //List[Int] = List(4, 4)

    val r2 = Apply[Option].lift2((_: Int) :: (_: List[Int])) //(Option[Int], Option[List[Int]]) => Option[List[Int]] = <function2>
    r2(3.some, List(4).some) //Option[List[Int]] = Some(List(3, 4))

    sequenceA(List(1.some, 2.some)) //Option[List[Int]] = Some(List(1, 2))
    sequenceA(List(3.some, none, 1.some)) //Option[List[Int]] = None
    sequenceA(List(List(1, 2, 3), List(4, 5, 6))) //List[List[Int]] = List(List(1, 4), List(1, 5), List(1, 6), List(2, 4), List(2, 5), List(2, 6), List(3, 4), List(3, 5), List(3, 6))

    type Function1Int[A] = ({type l[A]=Function1[Int, A]})#l[A] //defined type alias Function1Int
    // following should be ok in console
    //val r3 = sequenceA(List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1): List[Function1Int[Int]]) //Int => List[Int] = <function1>
    //r3(3) //List[Int] = List(6, 5, 4)
  }
}