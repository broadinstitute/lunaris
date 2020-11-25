import sbt.project

val lunarisV = "1.5.16"
val scalaV = "2.13.3"
val yootilzV = "0.1.5"
val scalaTestV = "3.2.0"
val betterFilesV = "3.9.1"
val googleCloudNioV = "0.121.0"
val circeV = "0.13.0"
val scallopV = "3.4.0"
val akkaHttpV = "10.1.12"
val akkaStreamV = "2.6.5"
val fastParseV = "2.2.2"
val configV = "1.4.0"

lazy val mainDeps = Seq(
  "org.broadinstitute" %% "yootilz-core" % yootilzV,
  "org.broadinstitute" %% "yootilz-gcp" % yootilzV,
  "com.github.pathikrit" %% "better-files" % betterFilesV,
  "com.google.cloud" % "google-cloud-nio" % googleCloudNioV,
  "io.circe" %% "circe-core" % circeV,
  "io.circe" %% "circe-generic" % circeV,
  "io.circe" %% "circe-parser" % circeV,
  "org.rogach" %% "scallop" % scallopV,
  "com.typesafe.akka" %% "akka-http"   % akkaHttpV,
  "com.typesafe.akka" %% "akka-stream" % akkaStreamV,
  "com.lihaoyi" %% "fastparse" % fastParseV,
  "com.typesafe" % "config" % configV
)

lazy val testDeps = Set(
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "lunaris",
    name in Linux := "lunaris",
    version := lunarisV,
    scalaVersion := scalaV,
    libraryDependencies ++= (mainDeps ++ testDeps),
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),
    mainClass in (Compile, run) := Some("lunaris.app.Lunaris"),
    maintainer := "Oliver A Ruebenacker <oliverr@broadinstitute.org>",
    packageSummary := "Streaming location-sorted, tabix-indexed, block-gzipped TSV files from Terra (Google Cloud Storage) for analysis",
    packageDescription := "Streaming location-sorted, tabix-indexed, block-gzipped TSV files from Terra (Google Cloud Storage) for analysis",
    debianPackageDependencies := Seq("java8-runtime-headless"),
    debianNativeBuildOptions in Debian := Seq("-Zgzip", "-z3") // gzip compression at level 3
  ).enablePlugins(JavaAppPackaging, DebianPlugin)

