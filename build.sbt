enablePlugins(JavaAppPackaging)

name := "PvZ2LevelMaker"

version := "0.3.1"

scalaVersion := "2.13.2"

organization := "com.guncolony"

maintainer in Windows := "com.guncolony"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
)


libraryDependencies += "com.google.code.gson" % "gson" % "2.8.6"


assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}