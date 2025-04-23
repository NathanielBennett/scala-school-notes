package com.guardian.gcp

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.StorageOptions

import scala.jdk.CollectionConverters._

object DfpBucketCheker extends App {

  val storage = StorageOptions.newBuilder().build().getService
  val bucketName = "gu-datatech-raw-dfp-logs-prod"

  val blobs = storage
    .list(bucketName, BlobListOption.prefix("file=NetworkImpressions_276087/version=2016-10-07/"), BlobListOption.currentDirectory())
    .iterateAll
    .asScala
    .toList

  print(blobs.size)

}
