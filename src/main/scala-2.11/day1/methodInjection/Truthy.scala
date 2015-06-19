package day1.methodInjection

import scalaz._, Scalaz._

/**
 * Created by haha on 2015/6/19.
 */
object Truthy {

  trait CanTruthy[A] { self =>
    def truthys(a: A): Boolean
  }

  object CanTruthy {
//    def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
    def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
      def truthys(a: A): Boolean = f(a)
    }
  }

  trait CanTruthyOps[A] {
    def self: A
    implicit def F: CanTruthy[A]
    final def truthy: Boolean = F.truthys(self)
  }

  object ToCanTruthyOps {
    implicit def toCanTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
      new CanTruthyOps[A] {
        def self = v
        implicit def F: CanTruthy[A] = ev
      }
  }

  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
    case 0 => false
    case _ => true
  })

  implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
    case Nil => false
    case _ => true
  })

  implicit def nilCanTruthy = CanTruthy.truthys((_: Nil.type) => false)

  import ToCanTruthyOps._

  def truthyIf[A: CanTruthy, B, C](cond: A)(ifyes: => B)(ifno: => C) =
    if(cond.truthy) ifyes
    else ifno

  def main(args: Array[String]) {
    import ToCanTruthyOps._
    10.truthy.println
    List("aaa").truthy.println
    Nil.truthy.println
    truthyIf(1 |-> 5){"yes".println}{"no".println}
  }
}