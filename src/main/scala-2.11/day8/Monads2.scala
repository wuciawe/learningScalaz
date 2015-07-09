package day8

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/9.
 */
object Monads2 {
  def main(args: Array[String]) {
    // In Scalaz Monad extends Applicative, so there¡¯s no question that all monads
    // are functors. This means we can use map or <*> operator.

    //It turns out that any nested monadic value can be flattened and
    // that this is actually a property unique to monads. For this, the
    // join function exists.
    // In Scalaz join (and its symbolic alias ¦Ì) is a method introduced by Bind.

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
  }

}
