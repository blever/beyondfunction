name := "beyondfunction"

version := "1.0.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"    % "7.0.0",
  "org.scalacheck"  %% "scalacheck"     % "1.10+"     % "test",
  "org.specs2"      %% "specs2"         % "2.0-RC1"   % "test"
)

resolvers ++= Seq(
  "sonatype"     at "http://oss.sonatype.org/content/repositories/releases/"
)
