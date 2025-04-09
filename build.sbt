name := "scala-school-notes"

version := "0.1"

scalaVersion := "2.13.6"



libraryDependencies ++= Seq(
  "com.google.cloud" % "google-cloud-storage" % "2.43.2",
  "com.google.cloud" % "google-cloud-bigquery" % "2.43.2",
  "org.typelevel" %% "cats-core" % "2.8.0",
  "com.lihaoyi" %% "requests" % "0.8.0",
  "software.amazon.awssdk" % "s3" % "2.31.12",
  "org.apache.httpcomponents" % "httpclient" % "4.5.9",
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "io.circe" %% "circe-parser" % "0.14.10",
  "io.circe" %% "circe-generic" % "0.14.10",
  "org.typelevel" %% "cats-core" % "2.6.1"

)



resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"


/*


It is a dependences issue, I fixed it adding a newer version of httpclient in pom. 4.5.9 which overides the 4.5.6.

<dependency>
           <groupId>org.apache.httpcomponents</groupId>
            <artifactId>httpclient</artifactId>
            <version>4.5.9</version>
        </dependency>


 */