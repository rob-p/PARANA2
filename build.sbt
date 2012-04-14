name := "PARANA"

version := "2.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers ++= Seq(
"sonatype.repo" at "https://oss.sonatype.org/content/groups/public",
"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
"Maven Repository" at "http://repo1.maven.org/maven2",
"JGraphT Repository" at "http://maven.irisa.fr/artifactory/list/repo/",
"Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
"librob" at "http://rob-p.github.com/scala-utils"
)

libraryDependencies  ++= Seq(
            // other dependencies here
            // "jgrapht" % "jgrapht" % "0.8.2",
            "com.assembla.scala-incubator" %% "graph-core" % "1.4.2",
	    "org.scalatest" %% "scalatest" % "1.6.1" % "test",
        "com.github.scala-incubator.io" %% "scala-io-core" % "0.4-SNAPSHOT",
	    "com.github.scala-incubator.io" %% "scala-io-core" % "0.4-SNAPSHOT",
	    "com.github.scopt" %% "scopt" % "1.1.2",
            "net.robpatro" %% "scala-utils" % "1.0.0"
)
