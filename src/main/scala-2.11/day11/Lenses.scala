package day11

import scalaz._
import Scalaz._

/**
 * Created by jwhu on 7/11/15.
 */
object Lenses {
  def main(args: Array[String]) {
    case class Point(x: Double, y: Double)
    case class Color(r: Byte, g: Byte, b: Byte)
    case class Turtle(position: Point, heading: Double, color: Color) {
      def forward(dist: Double): Turtle =
        copy(position =
          position.copy(
            x = position.x + dist * math.cos(heading),
            y = position.y + dist * math.sin(heading)
          ))
    }
    val turtle = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte)) //Turtle = Turtle(Point(2.0,3.0),0.0,Color(-1,-1,-1))
    turtle.forward(10) //Turtle = Turtle(Point(12.0,3.0),0.0,Color(-1,-1,-1))

    // modify content of child data structures

//    // imperative
//    a.b.c.d.e += 1
//    // functional
//    a.copy(
//      b = a.b.copy(
//        c = a.b.c.copy(
//          d = a.b.c.d.copy(
//            e = a.b.c.d.e + 1
//          ))))

    // Lens is to get rid of unnecessary copy calls.
    // Lens is a type alias for LensT[Id, A, B] like many other typeclasses.
    // And Store, which looks like a wrapper for setter A => B => A and getter A => B.

    // Using Lens
    val turtlePosition = Lens.lensu[Turtle, Point] (
      (a, value) => a.copy(position = value),
      _.position
    )
    val pointX = Lens.lensu[Point, Double] (
      (a, value) => a.copy(x = value),
      _.x
    )
    val turtleX = turtlePosition >=> pointX
    turtleX.get(turtle) //scalaz.Id.Id[Double] = 2.0
    turtleX.set(turtle, 5.0) //scalaz.Id.Id[Turtle] = Turtle(Point(5.0,3.0),0.0,Color(-1,-1,-1))
    turtleX.mod(_ + 1.0, turtle) //scalaz.Id.Id[Turtle] = Turtle(Point(3.0,3.0),0.0,Color(-1,-1,-1))
    // =>= is a symbolic variation for mod
    val incX0 = turtleX =>= {_ + 1.0}
    incX0(turtle) //scalaz.Id.Id[Turtle] = Turtle(Point(3.0,3.0),0.0,Color(-1,-1,-1))


    // Lens as a State Monad
    // %= method takes a function B => B and returns a State monad that expresses the change.
    val incX = for {
      x <- turtleX %= {_ + 1.0}
    } yield x
    incX(turtle) //(Turtle, Double) = (Turtle(Point(3.0,3.0),0.0,Color(-1,-1,-1)),3.0)

    val turtleHeading = Lens.lensu[Turtle, Double] (
      (a, value) => a.copy(heading = value),
      _.heading
    )
    val pointY = Lens.lensu[Point, Double] (
      (a, value) => a.copy(y = value),
      _.y
    )
    val turtleY = turtlePosition >=> pointY

    // += is sugar syntax for  Numeric lenses of general %=
    def forward(dist: Double) = for {
      heading <- turtleHeading
      x <- turtleX += dist * math.cos(heading)
      y <- turtleY += dist * math.sin(heading)
    } yield (x, y)
    forward(10.0)(turtle) //(Turtle, (Double, Double)) = (Turtle(Point(12.0,3.0),0.0,Color(-1,-1,-1)),(12.0,3.0))
    forward(10.0) exec (turtle) //scalaz.Id.Id[Turtle] = Turtle(Point(12.0,3.0),0.0,Color(-1,-1,-1))

    // Lens Laws
    // lens laws are common sense
    // 0. if I get twice, I get the same answer
    // 1. if I get, then set it back, nothing changes.
    // 2. if I set, then get, I get what I set.
    // 3. if I set twice then get, I get the second thing I set.

  }
}
