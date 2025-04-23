package com.guardian.gcp

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{BucketLifecycleConfiguration, BucketVersioningStatus, Delete, DeleteBucketRequest, DeleteObjectRequest, DeleteObjectsRequest, ExpirationStatus, GetBucketLifecycleConfigurationRequest, GetBucketVersioningRequest, LifecycleRule, LifecycleRuleFilter, ListObjectVersionsRequest, ListObjectVersionsResponse, ListObjectsV2Request, ListObjectsV2Response, NoncurrentVersionExpiration, ObjectIdentifier, OptionalObjectAttributes, PutBucketLifecycleConfigurationRequest, PutBucketVersioningRequest, S3Response, VersioningConfiguration}

import java.time.LocalDate
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._





object SegmentChecketTwo extends App {
  implicit val executionContext: ExecutionContext = ExecutionContext.global

  val buckets = List(
    ("ophan-clean-contributions-payment-provider-data", LocalDate.parse("2022-12-15")),
    ("ophan-clean-consents-by-acquisition-date", LocalDate.parse("2021-06-08")),
    ("ophan-raw-appnexus", LocalDate.parse("2021-05-19")),
    ("ophan-raw-prebid-analytics", LocalDate.parse("2018-04-11")),
    ("ophan-clean-appnexus-advertisers", LocalDate.parse("2021-07-22")),
    ("ophan-raw-sonobi", LocalDate.parse("2022-04-29")),
    ("ophan-clean-content", LocalDate.parse("2022-03-08")),
    ("ophan-clean-atoms", LocalDate.parse("2022-02-15")),
    ("ophan-raw-content", LocalDate.parse("2022-03-08")),
    ("ophan-clean-master-contact-history", LocalDate.parse("2019-11-14")),
    ("ophan-clean-content-and-conversion", LocalDate.parse("2021-02-08")),
    ("ophan-clean-athena-query-data", LocalDate.parse("2022-06-19")),
    ("ophan-clean-table-monitor", LocalDate.parse("2022-06-21")),
    ("aws-glue-notebooks-021353022223-eu-west-1", LocalDate.parse("2018-02-23")),
    ("aws-glue-scripts-021353022223-eu-west-1", LocalDate.parse("2020-07-07")),
    ("ophan-data-lake-configuration", LocalDate.parse("2019-09-12")),
    ("ophan-clean-anomalies-alerting", LocalDate.parse("2022-06-19")),
    ("ophan-raw-newsstand", LocalDate.parse("2019-06-24")),
    ("ophan-raw-oracle-feeds", LocalDate.parse("2022-11-28")),
    ("ophan-clean-discussion-moderation-abusereport", LocalDate.parse("2018-01-05")),
    ("ophan-clean-discussion-moderation-action", LocalDate.parse("2018-01-05")),
    ("ophan-clean-discussion-moderation-sanction", LocalDate.parse("2018-01-05")),
    ("ophan-raw-affiliate-link", LocalDate.parse("2023-12-22")),
    ("ophan-clean-consents-summary", LocalDate.parse("2023-10-02")),
    ("ophan-clean-identity", LocalDate.parse("2023-10-02")),
    ("ophan-clean-marketing-email-events", LocalDate.parse("2020-01-09")),
    ("ophan-clean-marketing-countries", LocalDate.parse("2019-06-11")),
    ("ophan-raw-zuora-account", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-contact", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-payment", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-paymentmethod", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-rateplan", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-rateplancharge", LocalDate.parse("2023-03-27")),
    ("ophan-raw-zuora-rateplanchargetier", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-subscription", LocalDate.parse("2023-03-28")),
    ("ophan-raw-zuora-subscriptionamendment", LocalDate.parse("2023-03-28")),
    ("ophan-clean-subscriptions", LocalDate.parse("2023-03-27")),
    ("ophan-clean-guardian-weekly-price-rise", LocalDate.parse("2019-06-21")),
    ("ophan-clean-rapp", LocalDate.parse("2019-05-01")),
    ("ophan-clean-braze-newsletter-membership", LocalDate.parse("2024-09-11")),
    ("ophan-temp-braze-newsletter-membership", LocalDate.parse("2019-11-15")),
    ("ophan-clean-identity-braze", LocalDate.parse("2025-03-24")),
    ("ophan-temp-identity-braze", LocalDate.parse("2021-11-11")),
    ("ophan-clean-braze-metadata", LocalDate.parse("2024-08-14")),
    ("ophan-temp-braze-metadata", LocalDate.parse("2022-05-24")),
    ("ophan-clean-identity-braze-users-export", LocalDate.parse("2024-02-08")),
    ("ophan-temp-identity-braze-users-export", LocalDate.parse("2023-06-26")),
    ("ophan-clean-braze-system", LocalDate.parse("2021-01-18")),
    ("braze-currents-backfill", LocalDate.parse("2023-02-16")),
    ("ophan-clean-push-notification-event", LocalDate.parse("2022-07-04")),
    ("ophan-clean-push-notification", LocalDate.parse("2022-07-04")),
    ("ophan-clean-mobile-subscriptions", LocalDate.parse("2022-07-04")),
    ("ophan-clean-mobile-user-subscription", LocalDate.parse("2022-07-04")),
    ("ophan-clean-mobile-subscription-events", LocalDate.parse("2019-09-18")),
    ("ophan-clean-salesforce-contact", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-subscription", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-case", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-discount", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-card-expiry", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-direct-debit-mandate-failure", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-account", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-direct-debit-mandate", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-direct-debit-mandate-event", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-cancellation-survey", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-imovo-contract", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-digital-voucher", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-zuora-subscription-product-feature", LocalDate.parse("2022-11-15")),
    ("ophan-clean-salesforce-holiday-stop-request", LocalDate.parse("2022-11-15")),
  ).toMap

  val s3Client = S3Client.builder()
    .region(Region.EU_WEST_1)
    .build()

  def responseToIdentifier[S <: S3Response](responseList: List[S], acc: List[ObjectIdentifier] = List.empty)(transform: S => List[ObjectIdentifier]): List[ObjectIdentifier] = {
    responseList match {
      case Nil => acc
      case head :: tail => responseToIdentifier(tail, transform(head) ::: acc)(transform)
    }
  }

  def buildObjectIdentifier(key: String, maybeVersionId: Option[String] = None): ObjectIdentifier = {
    val baseBuilder = ObjectIdentifier.builder()
      .key(key)

    maybeVersionId.map {
      versionId => baseBuilder.versionId(versionId)
    }
    .getOrElse(baseBuilder)
    .build()
  }

  def listObjects(bucketName: String): List[ObjectIdentifier] = {
    val listObjectsRequest = ListObjectsV2Request
      .builder()
      .bucket(bucketName)
      .build()

    val response = s3Client.listObjectsV2Paginator(listObjectsRequest)
    responseToIdentifier[ListObjectsV2Response](response.iterator().asScala.toList) {
      case listObjectsV2Response =>
        listObjectsV2Response.contents().asScala.toList.map {
          s3 =>
            buildObjectIdentifier(s3.key)
        }
    }
  }

  def listVersions(bucketName: String): List[ObjectIdentifier] = {
    val listObjectVersionsRequest = ListObjectVersionsRequest
      .builder()
      .bucket(bucketName)
      .build()

    val response = s3Client.listObjectVersionsPaginator(listObjectVersionsRequest)
    responseToIdentifier[ListObjectVersionsResponse](response.iterator().asScala.toList) {
      case listObjectVersionsResponse =>
        val deleteMarkers = listObjectVersionsResponse.deleteMarkers.asScala.toList.map {
          deleteMarker => buildObjectIdentifier( deleteMarker.key, Some(deleteMarker.versionId) )
        }

        val versions = listObjectVersionsResponse.versions.asScala.toList.map { version =>
          buildObjectIdentifier( version.key, Some(version.versionId) )
        }
        deleteMarkers ++ versions
    }
  }

  def deleteObjects(bucketName: String, identiers: List[ObjectIdentifier]): Unit = {

    val delete = Delete.builder()
      .objects(identiers: _*)
      .build

    val deleteObjectsRequest = DeleteObjectsRequest.builder()
      .bucket(bucketName)
      .delete(delete)
      .build()

    val deleteRespse = s3Client.deleteObjects(deleteObjectsRequest)
  //  println(deleteRespse.toString)
    if (deleteRespse.hasErrors) {
      deleteRespse.errors.asScala.foreach {
        error => println(s"Error: ${error.key}: ~${error.message()}")
      }
    }
    if (deleteRespse.hasDeleted) println(s"Deleted ${deleteRespse.deleted.size} from $bucketName")
  }

  def deleteBucket(bucketName: String): Unit = {

    val deleteBucketRequest = DeleteBucketRequest
      .builder()
      .bucket(bucketName)
      .build

    val deleteBucketResponse = s3Client.deleteBucket(deleteBucketRequest)
    println(s"Bucket deletion for $bucketName $deleteBucketResponse")
  }

  def marshallObjectDeletion(bucketName: String, listToDelete: List[ObjectIdentifier], iteration: Int = 0) : Unit = {
    println(s"${bucketName} Deletion: List ${listToDelete.size}, Pass: $iteration")
    listToDelete match {
      case Nil =>
        println(s"Deleting $bucketName")
        deleteBucket(bucketName)
      case objectIdents =>
         deleteObjectIdentifers(bucketName, objectIdents)
         val nextIdentifiers = getIdentifers(bucketName)
         marshallObjectDeletion(bucketName, nextIdentifiers, iteration + 1)
    }
  }

  def getIdentifers(bucketName: String) = listObjects(bucketName) ++ listVersions(bucketName)

  def deleteObjectIdentifers(bucketName: String, identifiers: List[ObjectIdentifier] ): Unit = {

    println(s"Deleting: ${identifiers.size} from $bucketName ")

    identifiers.grouped(1000)
      .zipWithIndex
      .foreach {
        case (batch, i) =>
          println(s"Delete: $i, batchsize: ${batch.size}")
          deleteObjects(bucketName, batch)
      }
  }

  def enableVersioning(bucketName: String): Unit = {
    val enabled = isVersioningEnabled(bucketName)

    if(!enabled) {
      println("Versioning not enabled. Enabling")
      val versioningConfiguration = VersioningConfiguration
        .builder()
        .status(BucketVersioningStatus.ENABLED)
        .build()

      val putBucketVersioningRequest = PutBucketVersioningRequest
        .builder()
        .bucket(bucketName)
        .versioningConfiguration(versioningConfiguration)
        .build()

      s3Client.putBucketVersioning(putBucketVersioningRequest)
    }
    println("Done")
  }

  def isVersioningEnabled(bucketName: String):  Boolean = {
      val getBucketVersioningRequest = GetBucketVersioningRequest
        .builder()
        .bucket(bucketName)
        .build()

      val versioningRwsoponse = s3Client.getBucketVersioning(getBucketVersioningRequest)
      println(s"Versioninf for bucket $bucketName: ${versioningRwsoponse.statusAsString} ")
      versioningRwsoponse.status == BucketVersioningStatus.ENABLED
  }


  def marshallDeletion(bucketName: String): Future[(String, Int)] = Future {
      println(s"Hello $bucketName")
      enableVersioning(bucketName)
      marshallObjectDeletion(bucketName, getIdentifers(bucketName))
      val objectsTwo = listVersions(bucketName)
      println(s"Objects in $bucketName: ${objectsTwo.size}")
     (bucketName, objectsTwo.size)
  }

  val bucketsRemaining = buckets.filterNot{
    case (bucketName, _) => bucketName.startsWith("ophan-temp") || bucketName.startsWith("ophan-raw")}



val totals = bucketsRemaining.filter{ case (bucketName, _) => bucketName.startsWith("ophan-clean")}
    .toList


totals.headOption.foreach{
  case(bucket, _) =>
    val l = listObjects(bucket)
    println(s"$bucket: ${l.size}")
}


/*
   val futureTotal = Future.sequence(totals).map {
    realTotals => realTotals.foldLeft(0) {
      case(total, bucketAndTotal) =>
        val (bucket, bucketTotal) = bucketAndTotal
        println(s"Total for $bucket: $bucketTotal")
        total + bucketTotal
    }
  }

val total = Await.result(futureTotal, Duration.Inf)
  println(s"Done: $total")
*/
}

