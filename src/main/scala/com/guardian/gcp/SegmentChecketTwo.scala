package com.guardian.gcp

import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, Storage, StorageOptions}
import com.guardian.gcp.SegmentChecketTwo.s3Client
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{Delete, DeleteBucketRequest, DeleteObjectsRequest, ListObjectsV2Request, ObjectIdentifier}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

object SegmentChecketTwo extends App {

  val bucketName = "ophan-temp-braze-system"

  def countBucket(bucketName: String, acc: List[ObjectIdentifier] = List.empty, continuationToken: Option[String] = None): Unit = {



    val listObjectsRequest = ListObjectsV2Request
      .builder()
      .continuationToken(continuationToken.getOrElse(null))
      .bucket(bucketName)
      .build()

    val resoponse = s3Client.listObjectsV2(listObjectsRequest)
    Option(resoponse.nextContinuationToken) match {
      case None => println(s"There ar ${resoponse.contents.size + acc} in ${bucketName}")
      case Some(contT) =>
        println(s"More: $contT")
        countBucket(bucketName, acc + resoponse.contents.size, Some(contT))
    }
  }

  def listBucket(bucketName: String): List[ObjectIdentifier] = {
    val listObjectsRequest = ListObjectsV2Request
      .builder()
      .bucket(bucketName)
      .build()

    s3Client.listObjectsV2(listObjectsRequest)
      .contents()
      .asScala
      .toList
      .map {
        s3Object =>
          ObjectIdentifier.builder()
            .key(s3Object.key())
            .build
      }
  }

  val s3Client = S3Client.builder()
    .region(Region.EU_WEST_1)
    .build()

  countBucket(bucketName)

  val identifiers = listBucket(bucketName)

  if ( identifiers.nonEmpty && false ) {

    val delete = Delete.builder()
      .objects(identifiers: _*)
      .build()

    val deleteObjectsRequest = DeleteObjectsRequest.builder()
      .bucket(bucketName)
      .delete(delete)
      .build()

    println(s"Deleting ${identifiers.size} keys!")
    s3Client.deleteObjects(deleteObjectsRequest)
    println("Done!")

    val deleteBucketRequest = DeleteBucketRequest
      .builder()
      .bucket(bucketName)
      .build()

    println(s"Deletiing $bucketName")
    s3Client.deleteBucket(deleteBucketRequest)
    println(s"DeletED $bucketName")


  } else {
    println("Empty!")
  }
}
