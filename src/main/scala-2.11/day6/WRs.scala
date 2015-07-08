package day6

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object WRs {

  case class KnightPos(c: Int, r: Int) {
    def move: List[KnightPos] =
      for {
        KnightPos(c2, r2) <- List(KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
          KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
          KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
          KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2)) if ((1 |-> 8) contains c2) && ((1 |-> 8) contains r2)
      } yield KnightPos(c2, r2)
    def in3: List[KnightPos] =
      for {
        first <- move
        second <- first.move
        third <- second.move
      } yield third
    def canReachIn3(end: KnightPos): Boolean = in3 contains end
  }

  def main(args: Array[String]) {
    for {
      x <- 3.some
      y <- "!".some
    } yield x.shows + y
    // in the above for comprehension x.shows + y is a raw string,
    // and yield will force the return value to be a monadic value

    for {
      first <- KnightPos(6, 2).move
      second <- first.move
      third <- second.move
    } yield third
    // when the last element is a function which returns a monadic value,
    // we have to extract that value first

    // Writer
    implicit class PairOps[A, B: Monoid](pair: (A, B)) {
      def applyLog[C](f: A => (C, B)): (C, B) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log |+| newlog)
      }
    }

    def isBigGang(x: Int): (Boolean, String) =
      (x > 9, "Compared gang size to 9.")

    (3, "Smallish gang.") applyLog isBigGang

    // Writer attaches a monoid to a value
    3.set("Smallish gang.") //scalaz.Writer[String,Int] = scalaz.WriterTFunctions$$anon$26@477a0c05
    "something".tell //scalaz.Writer[String,Unit] = scalaz.WriterTFunctions$$anon$26@374de9cf
    MonadTell[Writer, String].point(3).run //(String, Int) = ("",3)

    def logNumber(x: Int): Writer[List[String], Int] =
      x.set(List("Got number: " + x.shows)) //scalaz.Writer[List[String],Int]
    def multWithLog: Writer[List[String], Int] = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b // scalaz.Writer[List[String],Int]
    multWithLog run //(List[String], Int) = (List(Got number: 3, Got number: 5),15)

    def gcd(a: Int, b: Int): Writer[List[String], Int] =
      if (b == 0) for { _ <- List("Finished with " + a.shows).tell } yield a
      else
//        List(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell >>= { _ => gcd(b, a % b) }
        for {
          _ <- List(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell
          r <- gcd(b, a % b)
        } yield r
    gcd(8, 3).run //(List[String], Int) = (List(8 mod 3 = 2, 3 mod 2 = 1, 2 mod 1 = 0, Finished with 1),1)

    def gcd2(a: Int, b: Int): Writer[Vector[String], Int] =
      if (b == 0) for { _ <- Vector("Finished with " + a.shows).tell } yield a
      else for {
        result <- gcd2(b, a % b)
        _ <- Vector(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell
      } yield result
    gcd(8, 3).run //(Vector[String], Int) = (Vector(Finished with 1, 2 mod 1 = 0, 3 mod 2 = 1, 8 mod 3 = 2),1)

    // performance benchmark
    def vectorFinalCountDown(x: Int): Writer[Vector[String], Unit] = {
      import annotation.tailrec
      @tailrec def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] = x match {
        case 0 => w >>= { _ => Vector("0").tell }
        case x => doFinalCountDown(x - 1, w >>= { _ =>
          Vector(x.shows).tell
        })
      }
      val t0 = System.currentTimeMillis
      val r = doFinalCountDown(x, Vector[String]().tell)
      val t1 = System.currentTimeMillis
      r >>= { _ => Vector((t1 - t0).shows + " msec").tell }
    }
    def listFinalCountDown(x: Int): Writer[List[String], Unit] = {
      import annotation.tailrec
      @tailrec def doFinalCountDown(x: Int, w: Writer[List[String], Unit]): Writer[List[String], Unit] = x match {
        case 0 => w >>= { _ => List("0").tell }
        case x => doFinalCountDown(x - 1, w >>= { _ =>
          List(x.shows).tell
        })
      }
      val t0 = System.currentTimeMillis
      val r = doFinalCountDown(x, List[String]().tell)
      val t1 = System.currentTimeMillis
      r >>= { _ => List((t1 - t0).shows + " msec").tell }
    }
    vectorFinalCountDown(10000).run._1.last
    listFinalCountDown(10000).run._1.last


    // Reader, function monad is often called reader monad,
    // since the functions get input from the common source
    val f = (_: Int) * 5
    val g = (_: Int) + 3
    (g map f)(8) //Int = 55

    val f2 = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
    f(3) //Int = 19

    val addStuff: Int => Int = for {
      a <- (_: Int) * 2
      b <- (_: Int) + 10
    } yield a + b
    addStuff(3) //Int = 19
  }
}