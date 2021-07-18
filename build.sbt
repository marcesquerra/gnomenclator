val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "Gnomenclator",
    version := "0.1.0",

    scalaVersion := scala3Version,
    nativeImageAgentMerge := true,
  )
