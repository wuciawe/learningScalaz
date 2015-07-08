package day7

import scalaz._
import Scalaz._

/**
 * Created by haha on 2015/7/8.
 */
object ApplicativeBuilder {
  def main(args: Array[String]) {
    ^(3.some, 5.some) { _ + _}
    // but this seems not work for functions or type constructors with two parameters

    // so use |@|
    (3.some |@| 5.some) {_ + _}
    val f = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
  }

}
