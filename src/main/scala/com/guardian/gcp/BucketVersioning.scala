package com.guardian.gcp

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{DeleteMarkerEntry, GetBucketLifecycleConfigurationRequest, GetBucketVersioningRequest, ListObjectVersionsRequest, ObjectVersion}
import sun.security.util.ObjectIdentifier

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object BucketVersioning extends App {

  val bucketName = "ophan-temp-braze-system"

  val s3Client = S3Client.builder()
    .region(Region.EU_WEST_1)
    .build()

 /* def objectVersions(
         bucketName: String, deleteAcc: List[ObjectIdentifier] = List.empty,
         versionAcc: List[ObjectIdentifier] = List.empty, nextKeyMarker: Option[String] = None,
         nextVersionIdNarker: Option[String] = None
 ): List[ObjectIdentifier] = {
     val listObjectVersionsRequest = ListObjectVersionsRequest.builder()
       .bucket(bucketName)
       .keyMarker(nextKeyMarker.getOrElse(null))
       .versionIdMarker(nextVersionIdNarker.getOrElse(null))
       .build()


  }

4*/

  val listObjectVersionsRequest = ListObjectVersionsRequest
    .builder()
    .bucket(bucketName)
    .build()

  val response = s3Client.listObjectVersions(listObjectVersionsRequest)
  response.versions().asScala.foreach {
    v =>
      println(v.toString)
  }
  /*
  println()
    private val deleteMarkers: mutable.Buffer[DeleteMarkerEntry] = response.deleteMarkers().asScala
    deleteMarkers.foreach {
      d => println(d.toString)
    }

  println(s"D: ${deleteMarkers.size}")
  println(s"Trun ${response.isTruncated}")
  println(s" Keymarker *${response.keyMarker()}*")
  println(s" NextMaker *${response.nextKeyMarker()}*")
  println(s" NextVers *${response.nextVersionIdMarker()}*")
  println(s" Version: ${response.versionIdMarker()}")
  */

}
