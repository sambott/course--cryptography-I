name := "week3"

version := "1.0"

scalaVersion := "2.11.7"


libraryDependencies ++= {
  val akkaVer = "2.4.1"
  val playVer = "2.5.0-M2"
  Seq(
    "com.typesafe.play" %% "play-test" % playVer % "test",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalatestplus" %% "play" % "1.2.0" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
    "com.typesafe.play" %% "play-netty-server" % playVer,
    "com.typesafe.akka" %% "akka-cluster-tools" % akkaVer,
    "com.typesafe.akka" %% "akka-actor" % akkaVer,
    "com.typesafe.akka" %% "akka-testkit" % akkaVer,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVer,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
    "org.slf4j" % "slf4j-simple" % "1.7.14"
  )
}
