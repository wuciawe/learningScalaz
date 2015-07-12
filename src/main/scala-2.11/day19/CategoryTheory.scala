package day19

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/12/15.
 */
object CategoryTheory {
  def main(args: Array[String]) {
    // Set
    sealed trait Person {}
    case object John extends Person {}
    case object Mary extends Person {}
    case object Sam extends Person {}

    val a: Set[Person] = Set[Person](John, Mary, Sam)

    sealed trait Breakfast {}
    case object Eggs extends Breakfast {}
    case object Oatmeal extends Breakfast {}
    case object Toast extends Breakfast {}
    case object Coffee extends Breakfast {}

    // arrow, map
    val favoriteBreakfast: Person => Breakfast = {
      case John => Eggs
      case Mary => Coffee
      case Sam  => Coffee
    }

    val favoritePerson: Person => Person = {
      case John => Mary
      case Mary => John
      case Sam  => Mary
    }

    // identity
    identity(John)

    // composition
    val favoritePersonsBreakfast = favoriteBreakfast compose favoritePerson

    // point
    val johnPoint: Unit => Person = { case () => John } //Unit => Person = <function1>
    val r0 = favoriteBreakfast compose johnPoint //Unit => Breakfast = <function1>
    r0(()) //Breakfast = Eggs

    // equality
    import org.scalacheck.{Prop, Arbitrary, Gen}

    def arrowEqualsProp[A, B](f: A => B, g: A => B)(implicit ev1: Equal[B], ev2: Arbitrary[A]): Prop =
      Prop.forAll { a: A =>
        f(a) === g(a)
      }
    implicit val arbPerson: Arbitrary[Person] = Arbitrary {
      Gen.oneOf(John, Mary, Sam)
    }
    implicit val breakfastEqual: Equal[Breakfast] = Equal.equalA[Breakfast]
    val r1 = arrowEqualsProp(favoriteBreakfast, favoritePersonsBreakfast)
    r1.check
    val r2 = arrowEqualsProp(favoriteBreakfast, favoriteBreakfast)
    r2.check

    // isomorphism
    sealed trait Family {}
    case object Mother extends Family {}
    case object Father extends Family {}
    case object Child extends Family {}

    sealed trait Relic {}
    case object Feather extends Relic {}
    case object Stone extends Relic {}
    case object Flower extends Relic {}

    import Isomorphism.<=>
    val isoFamilyRelic = new (Family <=> Relic) {
      val to: Family => Relic = {
        case Mother => Feather
        case Father => Stone
        case Child  => Flower
      }
      val from: Relic => Family = {
        case Feather => Mother
        case Stone   => Father
        case Flower  => Child
      }
    }

    implicit val familyEqual = Equal.equalA[Family]
    implicit val relicEqual = Equal.equalA[Relic]
    implicit val arbFamily: Arbitrary[Family] = Arbitrary {
      Gen.oneOf(Mother, Father, Child)
    }
    implicit val arbRelic: Arbitrary[Relic] = Arbitrary {
      Gen.oneOf(Feather, Stone, Flower)
    }

    val r3 = arrowEqualsProp(isoFamilyRelic.from compose isoFamilyRelic.to, identity[Family] _)
    r3.check
    val r4 = arrowEqualsProp(isoFamilyRelic.to compose isoFamilyRelic.from, identity[Relic] _)
    r4.check
  }
}
