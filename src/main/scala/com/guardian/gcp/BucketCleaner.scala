
package com.guardian.gcp
import com.google.cloud.storage.Storage.{BlobListOption, BlobSourceOption}
import com.google.cloud.storage.{Blob, BlobId, StorageOptions}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._

object BucketCleaner extends App {

  val transferredBlobPrefixPattern = """^segment-export/(\S+)/(\d{4}-\d{2}-\d{2})/(\S+)/(\S+\.gz)$""".r
  val runDate = LocalDate.of(2024, 12, 6).format(DateTimeFormatter.ISO_DATE)

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
    .flatMap { segmentBlob =>
      println(s"Getting todays blobs for ${segmentBlob.getName}")
      listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}$runDate/")))
    }
    .flatMap { blob =>
      blob.getName match {
        case transferredBlobPrefixPattern(segmentId, dateString, transactionId, file) => Some(segmentId, (blob, transactionId, file))
        case _ => None
      }
    }
    .toList
    .groupBy { case (segmentId, (_, _, _)) => segmentId }
    .toList
    .foldLeft(Future(List[(String, Int)]())) {  case (segmentTotals, (segmentId, blobsForSegment)) =>
            println(s"Transferring segment $segmentId from $srcBucketName to $destBucketName")
            val totalForSegment = Future {
              blobsForSegment.foldLeft(0) { case (totalForSegment, blobInfo) =>
                  val (_, (blob, transactionId, fileName)) = blobInfo
                  blob.copyTo(
                    destBucketName,
                    s"segment-export/$segmentId/$runDate/$transactionId/$fileName",
                  )
                  totalForSegment + 1
                }
             }
         (for {
           total <- totalForSegment
           totals <- segmentTotals
         } yield (segmentId, total) :: totals)
       }

  Await.result(transferredSegmentTotals, Duration.Inf).map { case(segment, total) => println(s"Segment: $segment, Total: $total") }

}
