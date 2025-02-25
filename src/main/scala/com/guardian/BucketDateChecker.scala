package com.guardian

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, StorageOptions}
import com.guardian.gcp.BucketCleaner.{listBlobs, runDate}

import scala.jdk.CollectionConverters._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object BucketDateChecker extends App {

  val transferredBlobPrefixPattern = """^segment-export/(\S+)/(\d{4}-\d{2}-\d{2})/(\S+)/(\S+\.gz)$""".r
  val runDate = LocalDate.of(2025, 2, 19).format(DateTimeFormatter.ISO_DATE)

  val storage = StorageOptions.newBuilder().build().getService
  val srcBucketName = "gu-datatech-raw-braze-segment-memberships-prod"
  val destBucketName = "gu-datatech-raw-braze-segment-memberships-code"

  def listBlobs(blobListOptions: List[BlobListOption]): List[Blob] = {
    storage.list(destBucketName, blobListOptions: _*)
      .iterateAll()
      .asScala
      .toList
  }

  val segmentBlobs = listBlobs(List(BlobListOption.prefix("segment-export/"), BlobListOption.currentDirectory()))

  val t = segmentBlobs.map { segmentBlob =>
    val blubs = listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}$runDate/"), BlobListOption.currentDirectory()))
    (segmentBlob.getName.split("/").last, blubs.map(_.getName))
  }
  .filter{ case(_, blobs) => !blobs.isEmpty  }


  println(s"${t.size}")

}