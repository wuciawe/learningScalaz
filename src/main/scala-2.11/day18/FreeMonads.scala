package day18

import scalaz._
import Scalaz._
import Free._

/**
 * Created by jwhu on 7/12/15.
 */

// In Scalaz version, Free constructor is called Free.Suspend and Pure is called Free.Return.
sealed trait CharToy[+Next]
object CharToy {
  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
      case o: CharOutput[A] => CharOutput(o.a, f(o.next))
      case b: CharBell[A]   => CharBell(f(b.next))
      case CharDone()       => CharDone()
    }
  }
  private def liftF[F[+_]: Functor, R](command: F[R]): Free[F, R] =
    Free.Suspend[F, R](Functor[F].map(command) { Free.Return[F, R](_) })
  def output(a: Char): Free[CharToy, Unit] =
    liftF[CharToy, Unit](CharOutput(a, ()))
  def bell: Free[CharToy, Unit] = liftF[CharToy, Unit](CharBell(()))
  def done: Free[CharToy, Unit] = liftF[CharToy, Unit](CharDone())
  def pointed[A](a: A) = Free.Return[CharToy, A](a)
}

object FreeMonads {
  import CharToy._

  def main(args: Array[String]) {
    // a toy language with three commands
    sealed trait Toy[+A, +Next]
    case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
    case class Bell[Next](next: Next) extends Toy[Nothing, Next]
    case class Done() extends Toy[Nothing, Nothing]

    Output('A', Done()) //Output[Char,Done] = Output(A,Done())
    Bell(Output('A', Done())) //Bell[Output[Char,Done]] = Bell(Output(A,Done()))


//    sealed trait CharToy[+Next]
//    object CharToy {
//      case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
//      case class CharBell[Next](next: Next) extends CharToy[Next]
//      case class CharDone() extends CharToy[Nothing]
//
//      def output[Next](a: Char, next: Next): CharToy[Next] = CharOutput(a, next)
//      def bell[Next](next: Next): CharToy[Next] = CharBell(next)
//      def done: CharToy[Nothing] = CharDone()
//    }
//
//    import CharToy._
//    output('A', done) //CharToy[CharToy[Nothing]] = CharOutput(A,CharDone())
//    bell(output('A', done)) //CharToy[CharToy[CharToy[Nothing]]] = CharBell(CharOutput(A,CharDone()))

    case class Fix[F[_]](f: F[Fix[F]])
    object Fix {
      def fix(toy: CharToy[Fix[CharToy]]) = Fix[CharToy](toy)
    }

    // to keep the type
//    import Fix._
//    fix(output('A', fix(done))) //Fix[CharToy] = Fix(CharOutput(A,Fix(CharDone())))
//    fix(bell(fix(output('A', fix(done))))) //Fix[CharToy] = Fix(CharBell(Fix(CharOutput(A,Fix(CharDone())))))

    // handle error
//    sealed trait FixE[F[_], E]
//    object FixE {
//      case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
//      case class Throwy[F[_], E](e: E) extends FixE[F, E]
//
//      def fix[E](toy: CharToy[FixE[CharToy, E]]): FixE[CharToy, E] = Fix[CharToy, E](toy)
//      def throwy[F[_], E](e: E): FixE[F, E] = Throwy(e)
//      def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])(f: E1 => FixE[F, E2]): FixE[F, E2] = ex match {
//        case Fix(x)    => Fix[F, E2](Functor[F].map(x) {catchy(_)(f)})
//        case Throwy(e) => f(e)
//      }
//    }
    // functor of CharToy
//    implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
//      def map[A, B](fa: CharToy[A])(f: A => B): CharToy[B] = fa match {
//        case o: CharOutput[A] => CharOutput(o.a, f(o.next))
//        case b: CharBell[A]   => CharBell(f(b.next))
//        case CharDone()       => CharDone()
//      }
//    }

//    import FixE._
//    case class IncompleteException()
//    def subroutine = fix[IncompleteException](
//      output('A',
//        throwy[CharToy, IncompleteException](IncompleteException())))
//    def program = catchy[CharToy, IncompleteException, Nothing](subroutine) { _ =>
//      fix[Nothing](bell(fix[Nothing](done)))
//    }

    val subroutine = output('A') //subroutine: scalaz.Free[CharToy,Unit] = Suspend(CharOutput(A,Return(())))

    val program = for {
      _ <- subroutine
      _ <- bell
      _ <- done
    } yield ()

    def showProgram[R: Show](p: Free[CharToy, R]): String =
      p.resume.fold({
        case CharOutput(a, next) =>
          "output " + Show[Char].shows(a) + "\n" + showProgram(next)
        case CharBell(next) =>
          "bell " + "\n" + showProgram(next)
        case CharDone() =>
          "done\n"
      },
      { r: R => "return " + Show[R].shows(r) + "\n" })
    showProgram(program)

    def pretty[R: Show](p: Free[CharToy, R]) = print(showProgram(p))
    pretty(output('A')) //output A\nreturn ()
    pretty(pointed('A') >>= output) //output A\nreturn ()
    pretty(output('A') >>= pointed) //output A\nreturn ()
    pretty((output('A') >> done) >> output('C')) //output A\ndone
    pretty(output('A') >> (done >> output('C'))) //output A\ndone

    // In other words, we can think of a free monad as just being a list of functors.
    // The Free constructor behaves like a Cons, prepending a functor to the list,
    // and the Pure constructor behaves like Nil, representing an empty list (i.e. no functors).

    // Free Monad
    // A model for any recursive data type with data at the leaves.
    // A free monad is an expression tree with variables at the leaves and flatMap is variable substitution.

    // Trampoline
    // Using Trampoline any program can be transformed into a stackless one.
    def even[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => return_(true)
        case x :: xs => suspend(odd(xs))
      }
    def odd[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => return_(false)
        case x :: xs => suspend(even(xs))
      }
    even(List(1, 2, 3)).run //Boolean = false

    even(0 |-> 3000).run //Boolean = false

    type FreeMonoid[A] = Free[({type λ[+α] = (A,α)})#λ, Unit]
    def cons[A](a: A): FreeMonoid[A] = Free.Suspend[({type λ[+α] = (A,α)})#λ, Unit]((a, Free.Return[({type λ[+α] = (A,α)})#λ, Unit](())))
    cons(1) //FreeMonoid[Int] = Suspend((1,Return(())))
    val r0 = cons(1) >>= {_ => cons(2)} //scalaz.Free[[+α](Int, α),Unit] = Gosub(Suspend((1,Return(()))),<function1>)
    def toList[A](list: FreeMonoid[A]): List[A] =
      list.resume.fold(
      { case (x: A, xs: FreeMonoid[A]) => x :: toList(xs) },
      { _ => Nil })
    toList(r0) //List[Int] = List(1, 2)
  }
}
