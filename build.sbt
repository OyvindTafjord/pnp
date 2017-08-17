import org.allenai.plugins.DockerBuildPlugin

organization := "org.allenai"

name := "pnp"

description := "Library for probabilistic neural programming"

version := "0.1.2"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "17.0",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.3",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.2.3",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.2.3",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models",
  "javax.servlet" % "javax.servlet-api" % "3.1.0",
  "net.sf.jopt-simple" % "jopt-simple" % "4.9",
  "org.apache.commons" % "commons-lang3" % "3.5",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.1.v20170120",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalatra" %% "scalatra" % "2.5.0",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "io.spray" %%  "spray-json" % "1.3.3"
)

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))

bintrayOrganization := Some("allenai")

bintrayRepository := "private"

fork := true

cancelable in Global := true

// Docker configuration
enablePlugins(DockerBuildPlugin)
dockerImageBase := "allenai-docker-private-docker.bintray.io/java-dynet"
dockerCopyMappings += ((file("lib"), "lib"))
dockerCopyMappings += ((file("data"), "data"))
dockerCopyMappings += ((file("experiments"), "experiments"))
// mainClass := Some("org.allenai.pnp.semparse.SemanticParserCli")
