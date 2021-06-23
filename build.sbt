lazy val roguelike =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings(
      name := "roguelike",
      version := "0.0.1",
      scalaVersion := "3.0.0",
      organization := "roguelike",
      libraryDependencies ++= Seq(
        "org.scalameta" %%% "munit" % "0.7.26" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      showCursor := true,
      title := "Indigo Roguelike!",
      gameAssetsDirectory := "assets",
      windowStartWidth := 550,
      windowStartHeight := 400,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe" % "0.8.2",
        "io.indigoengine" %%% "indigo"            % "0.8.2",
        "io.indigoengine" %%% "indigo-extras"     % "0.8.2"
      ),
      // scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) } // required for parcel.
    )
    .settings(
      Compile / sourceGenerators += Def.task {
        TileCharGen
          .gen(
            "DfTiles",
            "roguelike",
            (Compile / sourceManaged).value,
            10,
            10
          )
      }.taskValue
    )

// To use indigoBuild or indigoRun, first comment out:
// `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }`
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
