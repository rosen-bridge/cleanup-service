lazy val sonatypePublic = "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"
lazy val sonatypeReleases = "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
lazy val sonatypeSnapshots = "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers ++= Seq(Resolver.mavenLocal, sonatypeReleases, sonatypeSnapshots, Resolver.mavenCentral)

version := "1.0.0"
val appkit = "org.ergoplatform" %% "ergo-appkit" % "4.0.8"

libraryDependencies ++= Seq(
  appkit, (appkit % Test).classifier("tests").classifier("tests-sources"),
  "org.scalaj" %% "scalaj-http" % "2.4.2",
  "com.squareup.okhttp3" % "mockwebserver" % "4.9.3",
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "com.typesafe" % "config" % "1.4.2",
  "ch.qos.logback" % "logback-classic" % "1.2.11"
)

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

