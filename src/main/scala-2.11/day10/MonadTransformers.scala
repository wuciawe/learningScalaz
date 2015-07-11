package day10

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object MonadTransformers {
  def main(args: Array[String]) {
    // A monad transformer is similar to a regular monad,
    // but it’s not a standalone entity:
    // instead, it modifies the behaviour of an underlying monad.

    // Reader again
    def myName(step: String): Reader[String, String] = Reader {step + ", I am " + _}
    def localExample: Reader[String, (String, String, String)] = for {
      a <- myName("First")
      b <- myName("Second") >=> Reader { _ + "dy"}
      c <- myName("Third")
    } yield (a, b, c)
    localExample("Fred") //(String, String, String) = (First, I am Fred,Second, I am Freddy,Third, I am Fred)

    // get the configuration
    type ReaderTOption[A, B] = ReaderT[Option, A, B]
    object ReaderTOption extends KleisliInstances with KleisliFunctions {
      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
    }
    def configure0(key: String) = ReaderTOption[Map[String, String], String] {_.get(key)}

    def setupConnection = for {
      host <- configure0("host")
      user <- configure0("user")
      password <- configure0("password")
    } yield (host, user, password)
    val goodConfig = Map(
      "host" -> "demo.com",
      "user" -> "sa",
      "password" -> "****"
    )
    setupConnection(goodConfig) //Option[(String, String, String)] = Some((eed3si9n.com,sa,****))
    val badConfig = Map(
      "host" -> "example.com",
      "user" -> "sa"
    )
    setupConnection(badConfig) //Option[(String, String, String)] = None


    // Stacking multiple Monad Transformers
    // When we stack a monad transformer on a normal monad,
    // the result is another monad.
    // This suggests the possibility that we can again stack
    // a monad transformer on top of our combined monad,
    // to give a new monad, and in fact this is a common
    // thing to do.
    type StateTReaderTOption[C, S, A] = StateT[({type l[+X] = ReaderTOption[C, X]})#l, S, A]
    object StateTReaderTOption extends StateTInstances with StateTFunctions {
      def apply[C, S, A](f: S => (S, A)) = new StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
        def apply(s: S) = f(s).point[({type l[X] = ReaderTOption[C, X]})#l]
      }
      def get[C, S]: StateTReaderTOption[C, S, S] =
        StateTReaderTOption { s => (s, s) }
      def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
        StateTReaderTOption { _ => (s, ()) }
    }

    type Stack = List[Int]
    type Config = Map[String, String]
    val pop = StateTReaderTOption[Config, Stack, Int] {
      case x :: xs => (xs, x)
    }

    val pop: StateTReaderTOption[Config, Stack, Int] = {
      import StateTReaderTOption.{get, put}
      for {
        s <- get[Config, Stack]
        val (x :: xs) = s
        _ <- put(xs)
      } yield x
    }
    def push(x: Int): StateTReaderTOption[Config, Stack, Unit] = {
      import StateTReaderTOption.{get, put}
      for {
        xs <- get[Config, Stack]
        r <- put(x :: xs)
      } yield r
    }
    def stackManip: StateTReaderTOption[Config, Stack, Int] = for {
      _ <- push(3)
      a <- pop
      b <- pop
    } yield(b)
    stackManip(List(5, 8, 2, 1))(Map()) //Option[(Stack, Int)] = Some((List(8, 2, 1),5))

    def configure[S](key: String) = new StateTReaderTOption[Config, S, String] {
      def apply(s: S) = ReaderTOption[Config, (S, String)] { config: Config => config.get(key) map {(s, _)} }
    }
    def stackManip2: StateTReaderTOption[Config, Stack, Unit] = for {
      x <- configure("x")
      a <- push(x.toInt)
    } yield(a)
    stackManip2(List(5, 8, 2, 1))(Map("x" -> "7")) //Option[(Stack, Unit)] = Some((List(7, 5, 8, 2, 1),()))
    stackManip2(List(5, 8, 2, 1))(Map("y" -> "7")) //Option[(Stack, Unit)] = None
  }
}
