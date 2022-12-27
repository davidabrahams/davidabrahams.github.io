val scala3Version = "3.2.1"
val rewrite: Seq[String] = sys.props.get("scala.rewrite") match {
  case Some(s) => Seq("-rewrite", "-" + s)
  case None => Seq.empty
}

lazy val root = project
  .in(file("."))
  .settings(
    name := "blockjumper",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "2.3.0"
    ),
    scalacOptions ++= (Seq(
      "-encoding",
      "UTF-8",
      /* "-Yexplicit-nulls", */
      // case class instances not automatically derived, not a useful feature
      // "-language:strictEquality",
      "-Xfatal-warnings",
      "-Ykind-projector",
      "-deprecation",
      "-feature",
      "-source",
      "future",
      // can set rewrite option to "indent" or "new-syntax" from system
      // properties
    ) ++ rewrite)
  ).enablePlugins(ScalaJSPlugin)
