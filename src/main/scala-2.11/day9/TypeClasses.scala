package day9

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object TypeClasses {
  def main(args: Array[String]) {
    // implement a tree, whose equality is measured by type and content, not the heap location
    def freeTree: Tree[Char] =
      'P'.node(
        'O'.node(
          'L'.node('N'.leaf, 'T'.leaf),
          'Y'.node('S'.leaf, 'A'.leaf)),
        'L'.node(
          'W'.node('C'.leaf, 'R'.leaf),
          'A'.node('A'.leaf, 'C'.leaf)))
    // change W into P
    // nested pattern match
    def changeToP(tree: Tree[Char]): Tree[Char] = tree match {
      case Tree.Node(x, Stream(
      l, Tree.Node(y, Stream(
      Tree.Node(_, Stream(m, n)), r)))) =>
        x.node(l, y.node('P'.node(m, n), r))
    }

    // use Zipper
    // A zipper data structure represents a hole.
    // We have the current focus represented as tree,
    // but everything else that can construct the entire tree back up is also preserved.

    // get zipper
    freeTree.loc
    // more focus to W
    freeTree.loc.getChild(2) >>= {_.getChild(1)} >>= {_.getLabel.some} //Option[Char] = Some(W)
    // modify the label of W
    val newFocus = freeTree.loc.getChild(2) >>= {_.getChild(1)} >>= {_.modifyLabel({_ => 'P'}).some} //Option[scalaz.TreeLoc[Char]] = Some(scalaz.TreeLocFunctions$$anon$2@107a26d0)
    // reconstruct a tree
    newFocus.get.toTree //scalaz.Tree[Char] = <tree>
    newFocus.get.toTree.draw foreach {_.print} //P|O+- ||  L+- |  ||  |  N+- |  |  ||  |  T`- |  |  ||  Y`- |  |   |  S+-    |  |   |  A`-    |  |L`- |   P+-    ||     C+- |     ||     R`- |     |   A`-    |      A+-       |      C`-

    // Zipper for Stream
    Stream(1, 2, 3, 4) //scala.collection.immutable.Stream[Int] = Stream(1, ?)
    Stream(1, 2, 3, 4).toZipper //Option[scalaz.Zipper[Int]] = Some(Zipper(<lefts>, 1, <rights>))
    Stream(1, 2, 3, 4).toZipper >>= {_.next} //Option[scalaz.Zipper[Int]] = Some(Zipper(<lefts>, 2, <rights>))
    Stream(1, 2, 3, 4).toZipper >>= {_.next} >>= {_.next} //Option[scalaz.Zipper[Int]] = Some(Zipper(<lefts>, 3, <rights>))
    Stream(1, 2, 3, 4).toZipper >>= {_.next} >>= {_.next} >>= {_.previous} //Option[scalaz.Zipper[Int]] = Some(Zipper(<lefts>, 2, <rights>))

    // modify and then convert back to Stream
    (Stream(1, 2, 3, 4).toZipper >>= {_.next} >>= {_.next} >>= {_.modify {_ => 7}.some}).get.toStream.toList //res32: List[Int] = List(1, 2, 7, 4)
    // with for comprehesion
    for {
      z <- Stream(1, 2, 3, 4).toZipper
      n1 <- z.next
      n2 <- n1.next
    } yield { n2.modify {_ => 7} }
  }
}