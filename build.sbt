organization := "fr.iscpif"

name := "schelling-quantity"

version := "1.0"

scalaVersion := "2.11.6"

val monocleVersion = "1.1.1"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
)