package com.guardian.gcp

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{ListObjectVersionsRequest, ListObjectVersionsResponse, ObjectIdentifier}

import scala.jdk.CollectionConverters._

object ObjectVersions extends App {

  def toObjectIdentifiers( responsePages: List[ListObjectVersionsResponse], acc: List[ObjectIdentifier] = List.empty): List[ObjectIdentifier] = {
     responsePages match {
       case Nil => acc
       case head :: tail =>
         toObjectIdentifiers(
           tail,
           acc :::
             head.deleteMarkers.asScala.toList.map {
               deleteMarker =>
                 ObjectIdentifier.builder()
                   .key(deleteMarker.key)
                   .build
             }
           :::
             head.versions.asScala.toList.map {
               version =>
                 ObjectIdentifier.builder()
                   .key(version.key)
                   .build
             }
         )
     }
  }

  val bucketName = "ophan-temp-braze-system"
  val s3Client = S3Client.builder()
    .region(Region.EU_WEST_1)
    .build()

  val listObjectsPaginatorRequest = ListObjectVersionsRequest
    .builder()
    .bucket(bucketName)
    .build()

  val response = s3Client.listObjectVersionsPaginator(listObjectsPaginatorRequest)
  val pages = response.iterator().asScala.toList

  println(s"Pages: ${pages.size}")
 /*
  println(s"Delete markers: ${response.deleteMarkers.stream.count()}")
  println(s"Version: ${response.versions().stream().count}")
*/

  val ids = toObjectIdentifiers(pages)
  println(s"Ids: ${ids.size}")
}
