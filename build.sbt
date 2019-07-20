name := "prog-mob_aggregate-programming"

version := "0.1"

scalaVersion := "2.11.0"

val scafi_version = "0.3.2"

val scafi_core  = "it.unibo.apice.scafiteam" %% "scafi-core"  % scafi_version
val scafi_simulator  = "it.unibo.apice.scafiteam" %% "scafi-simulator"  % scafi_version
val scafi_simulator_gui  = "it.unibo.apice.scafiteam" %% "scafi-simulator-gui"  % scafi_version
val scafi_platform = "it.unibo.apice.scafiteam" %% "scafi-distributed"  % scafi_version
val alchemist = "it.unibo.alchemist" % "alchemist-maps" % "8.2.2"


libraryDependencies ++= Seq(scafi_core, scafi_simulator_gui, scafi_platform, alchemist)