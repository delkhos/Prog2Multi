libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.json" % "json" % "20210307"

fork := true

javaOptions ++= Seq(
    "-Dsun.java2d.opengl=true"
)
