package com.guardian.gcp

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.StorageOptions

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._

object BucketCleanerThree extends App {

  val runDate = LocalDate.of(2024, 10, 15).format(DateTimeFormatter.ISO_DATE)
  def allSegmentsId: String = "e5d36a40-e85f-4177-b226-5abd3f89bf7f"
  val storage = StorageOptions.newBuilder().build().getService
  val bucketName = "gu-datatech-raw-braze-segment-memberships-code"
  implicit val ec = ExecutionContext.global

  val transferredBlobPrefixPattern = """^segment-export/(\S+)/(\d{4}-\d{2}-\d{2})/(\S+)/(\S+\.gz)$""".r

  val transferredSegmentBlobs = storage
    .list(bucketName, BlobListOption.prefix("segment-export"))
    .iterateAll
    .asScala
    .toList
 println(s"Raw: ${transferredSegmentBlobs.length}")


  val filtered =   transferredSegmentBlobs.filter { blob =>
      //Today and not all segments
      blob.getName.contains(runDate)
    }

  println(s"Filtered ${filtered.length}")
  val selected = filtered
    .flatMap { blob =>
      blob.getName match {
        case transferredBlobPrefixPattern(segmentId, _, transactionId, file) =>
          Some(segmentId, blob, transactionId, file)
        case _ => None
      }
    }
    .toList

    println(s"select: ${selected.length}")
   val grp  = selected.groupBy { case (segmentId, _, _, _) => segmentId }
    .toList
    //Tidy so as the segmentId isn'r repeated. Can't seem to find a better way of doing this
    .map {
      case (segmentId, blobsWithPrefixParts) =>
        val blobsTransactionsFiles = blobsWithPrefixParts.map { case (_, blob, trans, file) => (blob, trans, file) }
        (segmentId, blobsTransactionsFiles)
    }

    println(s"Grp:  ${grp.length}")

    val transferredSegmentTotals =  grp.foldLeft(Future(List[(String, Int)]())) {
        case (futureSegmentTotals, (segmentId, blobsForSegment)) =>
          println(s"Transferring segment $segmentId")
          val totalForSegment = blobsForSegment.foldLeft(Future[Int](0)) {
            case (totalForSegment, blobInfo) =>
              Future {
                val (blob, transactionId, fileName) = blobInfo
                val destPrefix = s"segment-export-bk/$runDate/$segmentId/$transactionId/$fileName"
                blob.copyTo(bucketName, destPrefix)
                totalForSegment.map(t => t + 1)
              }.flatten
          }
          (for {
            total <- totalForSegment
            totals <- futureSegmentTotals
          } yield (segmentId, total) :: totals)
      }

  val totalFilesCopied = {

    //This isn't a webserver so I'm saying the await is cool
    Await.result(transferredSegmentTotals, Duration.Inf).foldLeft(0) {
      case (total, (segment, totalForSegment)) =>
        println(s"Segment: $segment, Total: $totalForSegment")
        total + totalForSegment
    }
  }

  println(s"Total files copied: $totalFilesCopied ")

}