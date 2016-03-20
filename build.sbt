name := "fun"
version := "1.0-SNAPSHOT"
organization := "fun"

scalaVersion := "2.11.7"
scalacOptions ++= Seq(
  "-deprecation",
  "-target:jvm-1.8",
  "-Xlint",
  "-feature",
  "-optimize",
  "-Yclosure-elim",
  "-Yinline"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "org.slf4j" % "slf4j-api" % "1.7.19",
  "ch.qos.logback" % "logback-classic" % "1.1.6",
  "org.slf4j" % "jcl-over-slf4j" % "1.7.19"
)

mappings in (Compile, packageBin) ++= Seq(
  (baseDirectory.value / "natives" / "linux" / "libjinput-linux.so") -> "linux/libjinput-linux.so",
  (baseDirectory.value / "natives" / "linux" / "libjinput-linux64.so") -> "linux/libjinput-linux64.so",
  (baseDirectory.value / "natives" / "linux" / "liblwjgl.so") -> "linux/liblwjgl.so",
  (baseDirectory.value / "natives" / "linux" / "liblwjgl64.so") -> "linux/liblwjgl64.so",
  (baseDirectory.value / "natives" / "linux" / "libopenal.so") -> "linux/libopenal.so",
  (baseDirectory.value / "natives" / "linux" / "libopenal64.so") -> "linux/libopenal64.so",
  (baseDirectory.value / "natives" / "macosx" / "libjinput-osx.dylib") -> "macosx/libjinput-osx.dylib",
  (baseDirectory.value / "natives" / "macosx" / "libjinput-osx64.dylib") -> "macosx/libjinput-osx64.dylib",
  (baseDirectory.value / "natives" / "macosx" / "liblwjgl.dylib") -> "macosx/liblwjgl.dylib",
  (baseDirectory.value / "natives" / "macosx" / "liblwjgl64.dylib") -> "macosx/liblwjgl64.dylib",
  (baseDirectory.value / "natives" / "macosx" / "openal.dylib") -> "macosx/openal.dylib",
  (baseDirectory.value / "natives" / "macosx" / "openal64.dylib") -> "macosx/openal64.dylib",
  (baseDirectory.value / "natives" / "windows" / "jinput-dx8.dll") -> "windows/jinput-dx8.dll",
  (baseDirectory.value / "natives" / "windows" / "jinput-dx8_64.dll") -> "windows/jinput-dx8_64.dll",
  (baseDirectory.value / "natives" / "windows" / "jinput-raw.dll") -> "windows/jinput-raw.dll",
  (baseDirectory.value / "natives" / "windows" / "jinput-raw_64.dll") -> "windows/jinput-raw_64.dll",
  (baseDirectory.value / "natives" / "windows" / "lwjgl.dll") -> "windows/lwjgl.dll",
  (baseDirectory.value / "natives" / "windows" / "lwjgl64.dll") -> "windows/lwjgl64.dll",
  (baseDirectory.value / "natives" / "windows" / "OpenAL32.dll") -> "windows/OpenAL32.dll",
  (baseDirectory.value / "natives" / "windows" / "OpenAL64.dll") -> "windows/OpenAL64.dll"
)

test in assembly := {}
mainClass in assembly := Some("demos.TestVboShadersApp")
jarName in assembly := s"${name.value}-${version.value}.jar"

scapegoatConsoleOutput := false
