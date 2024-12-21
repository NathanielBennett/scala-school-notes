package com.guardian.advent

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, StorageOptions}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext

object SegmentChecker extends App  {


  val storage = StorageOptions.newBuilder().build().getService
  val srcBucketName = "gu-datatech-raw-braze-segment-memberships-prod"
  implicit val ec = ExecutionContext.global
  val runDate = LocalDate.of(2024, 12, 19).format(DateTimeFormatter.ISO_DATE)

  def listBlobs(blobListOptions: List[BlobListOption]): List[Blob] = {
    storage.list(srcBucketName, blobListOptions:_*)
      .iterateAll()
      .asScala
      .toList
  }

  val gcpSegments = listBlobs(List(BlobListOption.prefix(s"segment-export/"), BlobListOption.currentDirectory()))
    .flatMap { segmentBlob =>
      println(s"Getting todays blobs for ${segmentBlob.getName}")
      listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}$runDate/"), BlobListOption.currentDirectory()))
    }
   .flatMap { s => s.getName.split("/").toList.drop(1).headOption }






 }
