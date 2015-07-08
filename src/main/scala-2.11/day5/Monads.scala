package day5

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object Monads {
  def main(args: Array[String]) {
    3.some flatMap { x => (x + 1).some } //Option[Int] = Some(4)
    (none: Option[Int]) flatMap { x => (x + 1).some } //Option[Int] = None

    Monad[Option].point("WHAT") //Option[String] = Some(WHAT)
    9.some flatMap { x => Monad[Option].point(x * 10) } //Option[Int] = Some(90)
    (none: Option[Int]) flatMap { x => Monad[Option].point(x * 10) } //Option[Int] = None

    // example of balanced pole
    type Birds = Int
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else none
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else none
      def banana: Option[Pole] = none
    }

    Pole(0, 0).landLeft(2) //Pole = Pole(2,0)
    // and foldMap
    Pole(0, 0).landRight(1) flatMap {_.landLeft(2)} //Option[Pole] = Some(Pole(2,1))
    (none: Option[Pole]) flatMap {_.landLeft(2)} //Option[Pole] = None
    Monad[Option].point(Pole(0, 0)) flatMap {_.landRight(2)} flatMap {_.landLeft(2)} flatMap {_.landRight(2)} //Option[Pole] = Some(Pole(2,4))
    // monadic chaining
    Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)} //Option[Pole] = Some(Pole(2,4))
    Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)} //Option[Pole] = None
    Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.banana} >>= {_.landRight(1)} //Option[Pole] = None

    // >> function return a predetermined monadic value and ignore the input
    (none: Option[Int]) >> 3.some //Option[Int] = None
    3.some >> 4.some //Option[Int] = Some(4)
    3.some >> (none: Option[Int]) //Option[Int] = None

    // because of the precedence rules of operator in scala, following line is wrong
    // Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >> (none: Option[Pole]) >>= {_.landRight(1)}
    // instead, write both of them are right
    Monad[Option].point(Pole(0, 0)).>>=({_.landLeft(1)}).>>(none: Option[Pole]).>>=({_.landRight(1)})
    (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)}) >> (none: Option[Pole]) >>= {_.landRight(1)}

    // nested lambda
    3.some >>= { x => "!".some >>= { y => (x.shows + y).some } } //Option[String] = Some(3!)
    // with >>= any part of the calculation can fail
    3.some >>= { x => (none: Option[String]) >>= { y => (x.shows + y).some } } //Option[String] = None
    // and we have for comprehension
    for {
      x <- 3.some
      y <- "!".some
    } yield (x.shows + y)

    def routine: Option[Pole] =
      for {
        start <- Monad[Option].point(Pole(0, 0))
        first <- start.landLeft(2)
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
    routine //Option[Pole] = Some(Pole(3,2))

    def routine2: Option[Pole] =
      for {
        start <- Monad[Option].point(Pole(0, 0))
        first <- start.landLeft(2)
        _ <- (none: Option[Pole])
        second <- first.landRight(2)
        third <- second.landLeft(1)
      } yield third
    routine2 //Option[Pole] = None

    // pattern matching in for comprehesion, success and failure
    for {
      (x :: xs) <- "hello".toList.some
    } yield x
    for {
      (x :: xs) <- "".toList.some
    } yield x

    // List Monad
    ^(List(1, 2, 3), List(10, 100, 100)) {_ * _} //List[Int] = List(10, 100, 100, 20, 200, 200, 30, 300, 300)
    List(3, 4, 5) >>= {x => List(x, -x)} //List[Int] = List(3, -3, 4, -4, 5, -5)
    for {
      n <- List(1, 2)
      ch <- List('a', 'b')
    } yield (n, ch)

    // monadPlus and guard function
    for {
      x <- 1 |-> 50 if x.shows contains '7'
    } yield x

    List(1, 2, 3) <+> List(4, 5, 6) //List[Int] = List(1, 2, 3, 4, 5, 6)
    (1 |-> 50) filter { x => x.shows contains '7' } //List[Int] = List(7, 17, 27, 37, 47)

    // Knight chess move
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

    KnightPos(6, 2).move //List[KnightPos] = List(KnightPos(8,1), KnightPos(8,3), KnightPos(4,1), KnightPos(4,3), KnightPos(7,4), KnightPos(5,4))
    KnightPos(8, 1).move //List[KnightPos] = List(KnightPos(6,2), KnightPos(7,3))
    KnightPos(6, 2) canReachIn3 KnightPos(6, 1) //Boolean = true
    KnightPos(6, 2) canReachIn3 KnightPos(7, 3) //Boolean = false

    // Monad Laws

    // Left Identity
    // (Monad[F].point(x) flatMap {f}) assert_=== f(x)
    (Monad[Option].point(3) >>= { x => (x + 100000).some }) assert_=== 3 |> { x => (x + 100000).some }
    // Right Identity
    // (m forMap {Monad[F].point(_)}) assert_=== m
    ("move on up".some flatMap {Monad[Option].point(_)}) assert_=== "move on up".some

    // Associative
    // (m flatMap f) flatMap g assert_=== m flatMap { x => f(x) flatMap {g} }
    Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)} //Option[Pole] = Some(Pole(2,4))
    Monad[Option].point(Pole(0, 0)) >>= { x =>
      x.landRight(2) >>= { y =>
        y.landLeft(2) >>= { z =>
          z.landRight(2)
        }
      }
    } //Option[Pole] = Some(Pole(2,4))

    // monad.laws[Option].check
  }
}