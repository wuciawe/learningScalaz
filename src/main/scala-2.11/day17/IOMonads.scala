package day17

import scalaz._
import Scalaz._
import effect._
import IO._
import iteratee._
import Iteratee._

/**
 * Created by jwhu on 7/11/15.
 */
object IOMonads {
  def main(args: Array[String]) {
    val action1 = for {
      _ <- putStrLn("Hello, world!")
    } yield ()
    action1.unsafePerformIO

    val action2 = IO {
      val source = scala.io.Source.fromFile("./README.md")
      source.getLines.toStream
    }
    action2.unsafePerformIO.toList

    def program: IO[Unit] = for {
      line <- readLn
      _    <- putStrLn(line)
    } yield ()
    // IO[Unit] is an instance of Monoid,
    // so we can re-use the monoid addition function |+|.
    (program |+| program).unsafePerformIO


    // Iteratee
    def counter[E]: Iteratee[E, Int] = {
      def step(acc: Int)(s: Input[E]): Iteratee[E, Int] =
        s(el = e => cont(step(acc + 1)),
          empty = cont(step(acc)),
          eof = done(acc, eofInput[E])
        )
      cont(step(0))
    }
    (counter[Int] &= enumerate(Stream(1, 2, 3))).run //scalaz.Id.Id[Int] = 3
    (length[Int, Id] &= enumerate(Stream(1, 2, 3))).run //scalaz.Scalaz.Id[Int] = 3

    // Composing Iteratees
    def drop1Keep1[E]: Iteratee[E, Option[E]] = for {
      _ <- drop[E, Id](1)
      x <- head[E, Id]
    } yield x
    def alternates[E]: Iteratee[E, Stream[E]] =
      repeatBuild[E, Option[E], Stream](drop1Keep1) map {_.flatten}
    (alternates[Int] &= enumerate(Stream.range(1, 15))).run.force //scala.collection.immutable.Stream[Int] = Stream(2, 4, 6, 8, 10, 12, 14)

    import java.io._
    val r0 = enumReader[IO](new BufferedReader(new FileReader("./README.md")))
    (head[IoExceptionOr[Char], IO] &= r0).map(_ flatMap {_.toOption}).run.unsafePerformIO

    def lengthOfTwoFiles(f1: File, f2: File) = {
      val l1 = length[IoExceptionOr[Char], IO] &= enumReader[IO](new BufferedReader(new FileReader(f1)))
      val l2 = l1 &= enumReader[IO](new BufferedReader(new FileReader(f2)))
      l2.run
    }
    lengthOfTwoFiles(new File("./README.md"), new File("./TODO.txt")).unsafePerformIO //Int = 12731

    val readLn2 = takeWhile[Char, List](_ != '\n') flatMap (ln => drop[Char, Id](1).map(_ => ln))
    (readLn2 &= enumStream("Iteratees\nare\ncomposable".toStream)).run
    (collect[List[Char], List] %= readLn2.sequenceI &= enumStream("Iteratees\nare\ncomposable".toStream)).run //scalaz.Id.Id[List[List[Char]]] = List(List(I, t, e, r, a, t, e, e, s), List(a, r, e), List(c, o, m, p, o, s, a, b, l, e))

  }
}
