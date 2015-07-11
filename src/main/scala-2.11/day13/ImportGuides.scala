package day13

/**
 * Created by jwhu on 7/11/15.
 */
object ImportGuides {
  def main(args: Array[String]) {
    // Implicits
    // In Scala, imports are used for two purposes:
    // 1. To include names of values and types into the scope.
    // 2. To include implicits into the scope.
    // Implicits are for 4 purposes (probably):
    // 1. To provide typeclass instances.
    // 2. To inject methods and operators. (static monkey patching)
    // 3. To declare type constraints.
    // 4. To retrieve type information from compiler.
    // Implicits are selected in the following precedence:
    // 1. Values and converters accessible without prefix via
    // local declaration, imports, outer scope, inheritance, and
    // current package object.
    // Inner scope can shadow values when they are named the same.
    // 2. Implicit scope. Values and converters declared in
    // companion objects and package object of the type, its parts,
    // or super types.

    // import scalaz._

    // First, the names. Typeclasses like Equal[A] and Functor[F[_]]
    // are implemented as trait, and are defined under scalaz package.
    // So instead of writing scalaz.Equal[A] we can write Equal[A].
    // Next, also the names, but type aliases. scalaz’s package object
    // declares most of the major type aliases like @@[T, Tag] and
    // Reader[E, A], which is treated as a specialization of
    // ReaderT transformer. Again, these can also be accessed as
    // scalaz.Reader[E, A] if you want.
    // Finally, idInstance is defined as typeclass instance of Id[A]
    // for Traverse[F[_]], Monad[F[_]] etc, but it’s not relevant.
    // By virtue of declaring an instance within its package object
    // it will be available, so importing doesn’t add much.

    // The merit of import scalaz._ is for convenience, and it’s optional.

    // import Scalaz._

//    package scalaz
//
//    object Scalaz
//      extends StateFunctions        // Functions related to the state monad
//      with syntax.ToTypeClassOps    // syntax associated with type classes
//      with syntax.ToDataOps         // syntax associated with Scalaz data structures
//      with std.AllInstances         // Type class instances for the standard library types
//      with std.AllFunctions         // Functions related to standard library types
//      with syntax.std.ToAllStdOps   // syntax associated with standard library types
//      with IdInstances              // Identity type and instances

    // This is quite a nice way of organizing the imports.
    // Scalaz object itself doesn’t define anything and
    // it just mixes in the traits.
  }
}
