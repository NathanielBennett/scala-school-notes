name := "scala-school-notes"

version := "0.1"

scalaVersion := "2.13.6"


libraryDependencies ++= Seq(
  "com.google.cloud" % "google-cloud-storage" % "2.10.0",
  "com.google.cloud" % "google-cloud-bigquery" % "2.6.0",
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.apache.httpcomponents" % "httpclient" % "4.5.9"
)


/*


It is a dependences issue, I fixed it adding a newer version of httpclient in pom. 4.5.9 which overides the 4.5.6.

<dependency>
           <groupId>org.apache.httpcomponents</groupId>
            <artifactId>httpclient</artifactId>
            <version>4.5.9</version>
        </dependency>


 */