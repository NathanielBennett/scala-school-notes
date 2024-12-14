package com.guardian.gcp

import com.google.cloud.storage.Bucket.BlobTargetOption
import com.google.cloud.storage.Storage.{BlobListOption, BlobSourceOption, CopyRequest}
import com.google.cloud.storage.{Blob, StorageOptions}
import com.guardian.gcp.futures.transform.FutureFlatMapEc

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._


object BucketCleanerTwo extends App {

  implicit class RichBlob(val blob: Blob) {
      def transactionTimestamp: Int = {
        val trannsactionId = blob.getName.split("/").toList.last
        trannsactionId.dropRight(1).split("-").last.toInt
      }
  }

  val runDate = LocalDate.of(2024, 11, 20).format(DateTimeFormatter.ISO_DATE)

  def allSegmentsId: String = "e5d36a40-e85f-4177-b226-5abd3f89bf7f"

  val storage = StorageOptions.newBuilder().build().getService
  val bucketName = "gu-datatech-raw-braze-segment-memberships-prod"
  val destBucketName = "gu-datatech-raw-braze-segment-memberships-code"
  implicit val ec = ExecutionContext.global


  def listBlobs(blobListOptions: List[BlobListOption]): List[Blob] = {
    storage.list(destBucketName, blobListOptions:_*)
      .iterateAll()
      .asScala
      .toList
  }

  val transferredBlobPrefixPattern = """^segment-export-bk/(\S+)/(\d{4}-\d{2}-\d{2})/(\S+)/(\S+\.gz)$""".r
  println(s"Hello $runDate")
  val segmentBlobs =  listBlobs(List(BlobListOption.prefix("segment-export-bk/"), BlobListOption.currentDirectory()))
    .flatMap { segmentBlob =>
      println(s"Seg: ${segmentBlob.getName}")
      val transactionBlobs = listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}$runDate/"), BlobListOption.currentDirectory()))
      transactionBlobs.sortBy(_.transactionTimestamp).lastOption
    }
    .flatMap{ transactionBlob =>
      listBlobs(List(BlobListOption.prefix(transactionBlob.getName)))
    }


//..foreach{ b => println(b.getName)}



}