lazy val sonatypePublic = "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"
lazy val sonatypeReleases = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
lazy val sonatypeSnapshots = "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers ++= Seq(Resolver.mavenLocal, sonatypeReleases, sonatypeSnapshots, Resolver.mavenCentral)

version := "1.0.0"
scalaVersion := "2.12.7"
val appkit = "org.ergoplatform" %% "ergo-appkit" % "4.0.8"

libraryDependencies ++= Seq(
  appkit, (appkit % Test).classifier("tests").classifier("tests-sources"),
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.squareup.okhttp3" % "mockwebserver" % "4.9.3",
  "com.typesafe" % "config" % "1.4.2",
  "ch.qos.logback" % "logback-classic" % "1.2.11",
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "org.scalatestplus" %% "mockito-3-4" % "3.2.10.0" % Test
)

ThisBuild / publishMavenStyle := true

Test / publishArtifact := false

