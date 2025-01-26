package com.guardian

import com.google.cloud.storage.{Blob, StorageOptions}
import com.google.cloud.storage.Storage.BlobListOption

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.JavaConverters._


object SegmentCheckerThree extends App {

  implicit class RichBlob(blob: Blob) {
    def getLastPartOfPath = blob.getName.split("/").last
  }

  val storage = StorageOptions.newBuilder().build().getService

  val bucketName: String = s"gu-datatech-raw-braze-segment-memberships-code"
  val runDate = LocalDate.of(2025, 1, 23).format(DateTimeFormatter.ISO_DATE)

  def latestPathsPerSegment: Map[String, String] =
    listBlobs(List(BlobListOption.prefix("segment-export/"), BlobListOption.currentDirectory())).flatMap {
      segmentBlob =>
        listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}${runDate}/"), BlobListOption.currentDirectory()))
          .sortWith { case (a, b) => timestampOf(a.getLastPartOfPath) > timestampOf(b.getLastPartOfPath) }
          .headOption
          .map { latest =>
            (segmentBlob.getLastPartOfPath, latest.getName)
          }
    }.toMap

  private def listBlobs(blobListOptions: List[BlobListOption]): List[Blob] = {
    storage
      .list(bucketName, blobListOptions: _*)
      .iterateAll()
      .asScala
      .toList
  }

  def timestampOf(exportPrefix: String) = exportPrefix.dropRight(1).split('-').last.toInt

    latestPathsPerSegment.foreach{ t => println(t)}

/*  listBlobs(List(BlobListOption.prefix("segment-export/"), BlobListOption.currentDirectory())).flatMap {
    segmentBlob =>
      listBlobs(List(BlobListOption.prefix(s"${segmentBlob.getName}${runDate}/"), BlobListOption.currentDirectory()))

  }
    .foreach( s => println(s))*/
}
