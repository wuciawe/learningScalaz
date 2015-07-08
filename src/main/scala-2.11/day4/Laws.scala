package day4

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object Laws {
  def main(args: Array[String]) {
    // Functor Laws
    // Identity Law
    List(1, 2, 3) map {identity} assert_=== List(1, 2, 3)
    // Map Composite Law
    (List(1, 2, 3) map {{(_: Int) * 3} map {(_: Int) + 1}}) assert_=== (List(1, 2, 3) map {(_: Int) * 3} map {(_: Int) + 1})
    // in test:console, run following code to check the property of a type
    //  functor.laws[List].check

    // Applicative Laws
    //  trait ApplicativeLaw extends FunctorLaw {
    //    def identityAp[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
    //      FA.equal(ap(fa)(point((a: A) => a)), fa)
    //    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A])(implicit FC: Equal[F[C]]) =
    //      FC.equal(ap(ap(fa)(fab))(fbc), ap(fa)(ap(fab)(ap(fbc)(point((bc: B => C) => (ab: A => B) => bc compose ab)))))
    //    def homomorphism[A, B](ab: A => B, a: A)(implicit FB: Equal[F[B]]): Boolean =
    //      FB.equal(ap(point(a))(point(ab)), point(ab(a)))
    //    def interchange[A, B](f: F[A => B], a: A)(implicit FB: Equal[F[B]]): Boolean =
    //      FB.equal(ap(point(a))(f), ap(f)(point((f: A => B) => f(a))))
    //  }

    // Semigroup Laws
    //  trait SemigroupLaw {
    //    def associative(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
    //      F.equal(append(f1, append(f2, f3)), append(append(f1, f2), f3))
    //  }
    1 * (2 * 3) assert_=== (1 * 2) * 3
    // semigroup.laws[Int @@ Tags.Multiplication].check

    // Monoid Laws
    //  trait MonoidLaw extends SemigroupLaw {
    //    def leftIdentity(a: F)(implicit F: Equal[F]) = F.equal(a, append(zero, a))
    //    def rightIdentity(a: F)(implicit F: Equal[F]) = F.equal(a, append(a, zero))
    //  }
    1 * 2 assert_=== 2
    2 * 1 assert_=== 2
    // following contains errors!
//    (Monoid[Int @@ Tags.Multiplication].zero |+| Tags.Multiplication(2)) assert_=== 2
//    (Tags.Multiplication(2) |+| Monoid[Int @@ Tags.Multiplication].zero) assert_=== 2
    // monoid.laws[Int @@ Tags.Multiplication].check

    // if we don't if the contents are monoids, discard the second value and keep the first one
    Tags.First('a'.some) |+| Tags.First('b'.some) //scalaz.@@[Option[Char],scalaz.Tags.First] = Some(a)
    Tags.First(none: Option[Char]) |+| Tags.First('b'.some) //scalaz.@@[Option[Char],scalaz.Tags.First] = Some(b)
    Tags.First('a'.some) |+| Tags.First(none: Option[Char]) //scalaz.@@[Option[Char],scalaz.Tags.First] = Some(a)
    // or keep the second one
    Tags.Last('a'.some) |+| Tags.Last('b'.some) //scalaz.@@[Option[Char],scalaz.Tags.Last] = Some(b)
    Tags.Last(none: Option[Char]) |+| Tags.Last('b'.some) //scalaz.@@[Option[Char],scalaz.Tags.Last] = Some(b)
    Tags.Last('a'.some) |+| Tags.Last(none: Option[Char]) //scalaz.@@[Option[Char],scalaz.Tags.Last] = Some(a)

    // Foldable
    List(1, 2, 3).foldRight (1) {_ * _} //Int = 6
    9.some.foldLeft(2) {_ + _} //Int = 11
    // here the function is to make the content a monoid
    List(1, 2, 3) foldMap {identity} //Int = 6
    List(true, false, true, true) foldMap {Tags.Disjunction.apply} //scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true
  }
}