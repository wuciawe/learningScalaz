package day16

import scalaz._
import Scalaz._
import effect._
import ST.{newVar, runST, newArr, returnST}

/**
 * Created by jwhu on 7/11/15.
 */
object STMonads {
  def main(args: Array[String]) {
//    def e1[S] = for {
//      x <- newVar[S](0)
//      r <- x mod {_ + 1}
//    } yield x
//    def e2[S]: ST[S, Int] = for {
//      x <- e1[S]
//      r <- x.read
//    } yield r
//    type ForallST[A] = Forall[({type λ[S] = ST[S, A]})#λ]
//    runST(new ForallST[Int] { def apply[S] = e2[S] }) //Int = 1

    // STArray
    class STTest {//extends Spec {
      type ForallST[A] = Forall[({type λ[S] = ST[S, A]})#λ]

//      "STRef" in {
        def e1[S] = for {
          x <- newVar[S](0)
          r <- x mod {_ + 1}
        } yield x
        def e2[S]: ST[S, Int] = for {
          x <- e1[S]
          r <- x.read
        } yield r
        runST(new ForallST[Int] { def apply[S] = e2[S] })// must be_===(1)
//      }

//      "STArray" in {
        def e3[S] = for {
          arr <- newArr[S, Boolean](3, true)
          _ <- arr.write(0, false)
          r <- arr.freeze
        } yield r
        runST(new ForallST[ImmutableArray[Boolean]] { def apply[S] = e3[S] }).toList// must be_===(
//          List(false, true, true))
//      }
    }

    // prime number
    def mapM[A, S, B](xs: List[A])(f: A => ST[S, B]): ST[S, List[B]] =
      Monad[({type λ[α] = ST[S, α]})#λ].sequence(xs map f)
    def sieve[S](n: Int) = for {
      arr <- newArr[S, Boolean](n + 1, true)
      _ <- arr.write(0, false)
      _ <- arr.write(1, false)
      val nsq = (math.sqrt(n.toDouble).toInt + 1)
      _ <- mapM (1 |-> nsq) { i =>
        for {
          x <- arr.read(i)
          _ <-
          if (x) mapM (i * i |--> (i, n)) { j => arr.write(j, false) }
          else returnST[S, List[Boolean]] {Nil}
        } yield ()
      }
      r <- arr.freeze
    } yield r
    type ForallST[A] = Forall[({type λ[S] = ST[S, A]})#λ]
    def prime(n: Int) =
      runST(new ForallST[ImmutableArray[Boolean]] { def apply[S] = sieve[S](n) }).toArray.
        zipWithIndex collect { case (true, x) => x }
    prime(1000)
  }
}
