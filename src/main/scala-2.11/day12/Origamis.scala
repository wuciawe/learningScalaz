package day12

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object Origamis {
  def main(args: Array[String]) {
    // What is Origami programming? I don't know, just follow the notes.

    // Unfolds
    // DList, difference list, which provides the unfoldr method, supporting constant time appending
    val r0 = DList.unfoldr(10, { (x: Int) => if (x == 0) none else (x, x - 1).some }) //scalaz.DList[Int] = scalaz.DListFunctions$$anon$3@70627153
    r0.toList //List[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

    // unfold for Stream
    val r1 = unfold(10) { (x) => if (x == 0) none else (x, x - 1).some } //Stream[Int] = Stream(10, ?)
    r1.toList //List[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

    // implementation for selection sort
    def minimumS[A: Order](stream: Stream[A]) = stream match {
      case x #:: xs => xs.foldLeft(x) {_ min _}
    }
    def deleteS[A: Equal](y: A, stream: Stream[A]): Stream[A] = (y, stream) match {
      case (_, Stream()) => Stream()
      case (y, x #:: xs) =>
        if (y === x) xs
        else x #:: deleteS(y, xs)
    }
    def delmin[A: Order](stream: Stream[A]): Option[(A, Stream[A])] = stream match {
      case Stream() => none
      case xs =>
        val y = minimumS(xs)
        (y, deleteS(y, xs)).some
    }
    def ssort[A: Order](stream: Stream[A]): Stream[A] = unfold(stream){delmin[A]}
    ssort(Stream(1, 3, 4, 2)).toList //List[Int] = List(1, 2, 3, 4)
  }
}
