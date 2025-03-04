name := "learningScalaz"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= {
  val scalazVersion = "7.1.2"
  Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
    "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
    "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion// % "test"
  )
}

scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-language:postfixOps", "-language:higherKinds", "-deprecation")

initialCommands in console := "import scalaz._, Scalaz._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"