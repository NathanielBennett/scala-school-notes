package com.guardian.gcp

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, Storage, StorageOptions}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

object SegmentChecketTwo extends App {

  val transferredBlobPrefixPattern = """^segment-export/(\S+)/(\d{4}-\d{2}-\d{2})/(\S+)/(\S+\.gz)$""".r
  val runDate = LocalDate.of(2025, 2, 13).format(DateTimeFormatter.ISO_DATE)

  val storage = StorageOptions.newBuilder().build().getService
  val srcBucketName = "gu-datatech-raw-braze-segment-memberships-prod"
  val destBucketName = "gu-datatech-raw-braze-segment-memberships-code"
  implicit val ec = ExecutionContext.global

  def listBlobs(blobListOptions: List[BlobListOption]): List[Blob] = {
    storage.list(srcBucketName, blobListOptions:_*)
      .iterateAll()
      .asScala
      .toList
  }

  val transferredSegmentTotals = listBlobs(List(BlobListOption.prefix("segment-export/"), BlobListOption.currentDirectory()))
    .foreach{ segment =>
       listBlobs(List(BlobListOption.prefix(s"${segment.getName}$runDate/")))
         .foreach { b =>
           val dest = b.getName
            println(s"copying: ${b.getName}" )
           b.copyTo(destBucketName, b.getName)

         }
    }
}
