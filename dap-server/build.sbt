val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "foo",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    // https://mvnrepository.com/artifact/org.eclipse.lsp4j/org.eclipse.lsp4j.debug
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j.debug" % "0.12.0",
    // https://mvnrepository.com/artifact/commons-io/commons-io
    libraryDependencies += "commons-io" % "commons-io" % "2.11.0"
)
    