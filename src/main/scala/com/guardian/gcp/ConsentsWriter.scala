package com.guardian.gcp

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.StorageOptions
import scala.jdk.CollectionConverters._

object ConsentsWriter extends App {

  val storage = StorageOptions.newBuilder().build().getService
  val bucket = "gu-datatech-raw-consent-prod"
  val destBucket = "gu-datatech-raw-consent-backfill-prod"

  val exports = storage
    .list(bucket)
    .iterateAll
    .asScala
    .filter {
      bl => bl.getName.startsWith("backfill/export")
    }
    .foreach { blob =>
      println(s"MovingL ${blob.getName}")
      blob.copyTo(destBucket, blob.getName)
    }


/*
  val orcs = exports.toList.flatMap{ bl =>
   // println(s"listing ${bl.getName}")
    storage
      .list(bucket, BlobListOption.prefix(bl.getName))
      .iterateAll
      .asScala
      .filter(_.getName.endsWith(".orc"))
      .toList
  }
  println(s"All ${orcs.size}")

  orcs.foreach {
    b => println(s"Copy: ${b.getName}")
    val prefix = s"backfill/${b.getName}"
    b.copyTo(bucket, prefix)
  }
*/
}
