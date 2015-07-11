package day12

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object IteratorPatterns {
  def main(args: Array[String]) {
    // breaking down the GoF Iterator pattern into two aspects: mapping and accumulating.
    // For applicative functors, it brings up the fact that there are three kinds of applicatives:
    // 1. Monadic applicative functors
    // 2. Naperian applicative functors
    // 3. Monoidal applicative functors

    // Naperian applicative functor zips together data structure that are fixed in shape.
    // Also apparently appliactive functors were originally named idiom,
    // so idiomatic in that paper means applicative.

    // Monoidal applicatives
    // Scalaz implements Monoid[m].applicative to turn any monoids into an applicative.
    Monoid[Int].applicative.ap2(1, 1)(0) //Int = 2
    Monoid[List[Int]].applicative.ap2(List(1), List(1))(Nil) //List[Int] = List(1, 1)

    // Combining applicative functors
    // Like monads, applicative functors are closed under products;
    // so two independent idiomatic effects can generally be fused into one, their product.
    Applicative[List].product[Option] //scalaz.Applicative[[α](List[α], Option[α])] = scalaz.Applicative$$anon$2@211b3c6a
    Applicative[List].product[Option].point(1) //(List[Int], Option[Int]) = (List(1),Some(1))
    // The product seems to be implemented as a Tuple2.
    ((List(1), 1.some) |@| (List(1), 1.some)) {_ |+| _} //(List[Int], Option[Int]) = (List(1, 1),Some(2))
    ((List(1), 1.success[String]) |@| (List(1), "boom".failure[Int])) {_ |+| _} //(List[Int], scalaz.Validation[String,Int]) = (List(1, 1),Failure(boom))

    // Unlike monads in general, applicative functors are also closed under composition;
    // so two sequentially-dependent idiomatic effects can generally be fused into one,
    // their composition.
    Applicative[List].compose[Option] //scalaz.Applicative[[α]List[Option[α]]] = scalaz.Applicative$$anon$1@461800f1
    Applicative[List].compose[Option].point(1) //List[Option[Int]] = List(Some(1))

    // The two operators ⊗ and ⊙ allow us to combine idiomatic computations in two different ways;
    // we call them parallel and sequential composition, respectively.

    // Idiomatic traversal
    List(1, 2, 3) traverse { x => (x > 0) option (x + 1) } //Option[List[Int]] = Some(List(2, 3, 4))
    List(1, 2, 0) traverse { x => (x > 0) option (x + 1) } //Option[List[Int]] = None
    // In the case of a monadic applicative functor,
    // traversal specialises to monadic map, and has the same uses.

    //For a monoidal applicative functor, traversal accumulates values.
    // The function reduce performs that accumulation,
    // given an argument that assigns a value to each element.
    Monoid[Int].applicative.traverse(List(1, 2, 3)) {_ + 1} //Int = 9

    // Shape and contents
    // In addition to being parametrically polymorphic in the collection elements,
    // the generic traverse operation is parametrised along two further dimensions:
    // the datatype being traversed,
    // and the applicative functor in which the traversal is interpreted.
    // Specialising the latter to lists as a monoid yields a generic contents operation.
    def contents[F[_]: Traverse, A](f: F[A]): List[A] =
      Monoid[List[A]].applicative.traverse(f) {List(_)}
    contents(List(1, 2, 3)) //List[Int] = List(1, 2, 3)
    contents(NonEmptyList(1, 2, 3)) //List[Int] = List(1, 2, 3)
    val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf) //scalaz.Tree[Char] = <tree>
    contents(tree) //List[Char] = List(P, O, L)
    // another implementation of contents
//    def contents[F[_]: Traverse, A](f: F[A]): List[A] =
//      f.traverse[({type l[X]=List[A]})#l, A] {List(_)}

    // The other half of the decomposition is obtained simply by a map,
    // which is to say, a traversal interpreted in the identity idiom.
    def shape[F[_]: Traverse, A](f: F[A]): F[Unit] =
      f traverse {_ => ((): Id[Unit])}
    shape(List(1, 2, 3)) //List[Unit] = List((), (), ())
    shape(tree).drawTree //String = ...

    // This pair of traversals nicely illustrates the two aspects of iterations that
    // we are focussing on, namely mapping and accumulation.

    // Let’s also implement decompose function:
    def decompose0[F[_]: Traverse, A](f: F[A]) = (shape(f), contents(f))
    decompose0(tree) //(scalaz.Tree[Unit], List[Char]) = (<tree>,List(P, O, L))
    // This traverses the tree twice. Remember a product of two applicatives are also an applicative?
    def decompose[F[_]: Traverse, A](f: F[A]) =
      Applicative[Id].product[({type l[X]=List[A]})#l].traverse(f) { x => (((): Id[Unit]), List(x)) }
    decompose(List(1, 2, 3, 4)) //(scalaz.Scalaz.Id[List[Unit]], List[Int]) = (List((), (), (), ()),List(1, 2, 3, 4))
    decompose(tree) //(scalaz.Scalaz.Id[scalaz.Tree[Unit]], List[Char]) = (<tree>,List(P, O, L))

    // Sequence method introduced by Traverse
    // Instead of Monad, the requirement is relaxed to Applicative.
    List(1.some, 2.some).sequence //Option[List[Int]] = Some(List(1, 2))
    List(1.some, 2.some, none).sequence //Option[List[Int]] = None
    val validationTree: Tree[Validation[String, Int]] =
      1.success[String].node(2.success[String].leaf, 3.success[String].leaf) //validationTree: scalaz.Tree[scalaz.Validation[String,Int]] = <tree>
    validationTree.sequence[({type l[X]=Validation[String, X]})#l, Int] //scalaz.Validation[String,scalaz.Unapply[scalaz.Traverse,scalaz.Tree[scalaz.Validation[String,Int]]]{type M[X] = scalaz.Tree[X]; type A = scalaz.Validation[String,Int]}#M[Int]] = Success(<tree>)
    val failedTree: Tree[Validation[String, Int]] =
      1.success[String].node(2.success[String].leaf, "boom".failure[Int].leaf) //failedTree: scalaz.Tree[scalaz.Validation[String,Int]] = <tree>
    failedTree.sequence[({type l[X]=Validation[String, X]})#l, Int] //res163: scalaz.Validation[String,scalaz.Unapply[scalaz.Traverse,scalaz.Tree[scalaz.Validation[String,Int]]]{type M[X] = scalaz.Tree[X]; type A = scalaz.Validation[String,Int]}#M[Int]] = Failure(boom)

    // Collection and dispersal
    // We have found it convenient to consider special cases of effectful traversals,
    // in which the mapping aspect is independent of the accumulation, and vice versa.
    // The ﬁrst of these traversals accumulates elements effectfully,
    // with an operation of type a → m (),
    // but modiﬁes those elements purely and independently of this accumulation,
    // with a function of type a → b.

    def collect[F[_]: Traverse, A, S, B](t: F[A])(f: A => B)(g: S => S) =
      t.traverseS[S, B] { a => State { (s: S) => (g(s), f(a)) } }
    val loop = collect(List(1, 2, 3, 4)) {(_: Int) * 2} {(_: Int) + 1} //scalaz.State[Int,scalaz.Unapply[scalaz.Traverse,List[Int]]{type M[X] = List[X]; type A = Int}#M[Int]] = scalaz.package$State$$anon$1@3926008a
    loop(0) //(Int, scalaz.Unapply[scalaz.Traverse,List[Int]]{type M[X] = List[X]; type A = Int}#M[Int]) = (4,List(2, 4, 6, 8))

    // The second kind of traversal modiﬁes elements purely but dependent on the state,
    // with a binary function of type a → b → c, evolving this state independently of the elements,
    // via a computation of type m b.

    def label[F[_]: Traverse, A](f: F[A]): F[Int] =
      (f.traverseS {_ => for {
        n <- get[Int]
        x <- put(n + 1)
      } yield n}) eval 0
    label(List(10, 2, 8)) //List[Int] = List(0, 1, 2)
    label(tree).drawTree //String = ...
  }
}
