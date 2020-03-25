import sbt.project

val lunarisV = "0.0.1"
val scalaV = "2.13.1"
val yootilzV = "0.1.2"
val scalaTestV = "3.1.1"
val betterFilesV = "3.8.0"
val googleCloudNioV = "0.107.0-alpha"

lazy val mainDeps = Seq(
  "org.broadinstitute" %% "yootilz-core" % yootilzV,
  "org.broadinstitute" %% "yootilz-gcp" % yootilzV,
  "com.github.pathikrit" %% "better-files" % betterFilesV,
  "com.google.cloud" % "google-cloud-nio" % googleCloudNioV
)

lazy val testDeps = Set(
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "lunaris",
    version := lunarisV,
    scalaVersion := scalaV,
    libraryDependencies ++= (mainDeps ++ testDeps),
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),
    mainClass in (Compile, run) := Some("lunaris.app.LunarisApp"),
    maintainer := "Oliver A Ruebenacker <oliverr@broadinstitute.org>",
    packageSummary := "Streaming location-sorted, tabix-indexed, block-gzipped TSV files from Terra (Google Cloud Storage) for analysis",
    packageDescription := "Streaming location-sorted, tabix-indexed, block-gzipped TSV files from Terra (Google Cloud Storage) for analysis",
    debianPackageDependencies := Seq("java8-runtime-headless"),
    debianNativeBuildOptions in Debian := Seq("-Zgzip", "-z3") // gzip compression at level 3
  ).enablePlugins(JavaAppPackaging, DebianPlugin)

