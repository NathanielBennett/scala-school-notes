package com.guardian.gcp

import org.threeten.bp.Instant
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.model.{Delete, DeleteBucketRequest, DeleteObjectRequest, DeleteObjectsRequest, GetBucketMetadataTableConfigurationRequest, ListBucketsRequest, ListObjectsV2Request, ObjectIdentifier, S3Object}
import software.amazon.awssdk.services.s3.{S3Client, model}

import java.time.{LocalDate, ZoneId}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

object SegmentChecker extends App {

  implicit val ec = ExecutionContext.global

  val s3Client = S3Client.builder()
    .region(Region.EU_WEST_1)
    .build()


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
    ("ophan-temp-braze-system", LocalDate.parse("2021-01-18")),
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

  def listBucket(bucketName: String): Iterable[ObjectIdentifier] = {
    val listObjectsRequest = ListObjectsV2Request
      .builder()
      .bucket(bucketName)
      .build()

    s3Client.listObjectsV2(listObjectsRequest).contents().asScala
      .map { s3Object =>
        ObjectIdentifier.builder()
          .key(s3Object.key)
          .build
      }
  }


   def emptyAndDelete(bucketName: String): Unit = {

     val identifiers = listBucket(bucketName)

     if (identifiers.nonEmpty) {
       println(s"deleting ${identifiers.size} for bucket: $bucketName ")

       val delete = Delete.builder()
         .objects(identifiers.toList: _*)
         .build()

       val deleteObjectsRequest = DeleteObjectsRequest
         .builder()
         .bucket(bucketName)
         .delete(delete)
         .build

       s3Client.deleteObjects(deleteObjectsRequest)

     }
   }


  val listBucketsRequest = ListBucketsRequest.builder()
    //.prefix("ophan-raw-salesforce")
    .build()

  s3Client.listBuckets(listBucketsRequest)
    .buckets()
    .asScala
    .flatMap{
      b => buckets.get(b.name()).map { lastModifed =>
          (b.name, lastModifed)
      }
    }
    .zipWithIndex
    .foreach { case (bucket, index) =>
      val (name, lastModified) = bucket
      val mostRecent = listBucket(name)
      if (mostRecent.isEmpty) println(s"Name $name: Last modified: $lastModified - EMPTY")
      else println(s"Non empty: $name")
    }
/*
   val bucketsListResponse = s3Client.listBuckets()
   bucketsListResponse.buckets().asScala.toList.foreach{
     bucket => buckets.get(bucket.name).foreach{ lastMod =>
       println(s"${bucket.name}: $lastMod ")
       println(s"Empty: ${isEmpty(bucket.name).isEmpty}")
     }
   }
*/

/*
  sfBuckets.foreach {
    bucket =>
      println(s"Deleting ${bucket.name}")
      val deleteBucketRequest = DeleteBucketRequest
        .builder()
        .bucket(bucket.name)
        .build()
      s3Client.deleteBucket(deleteBucketRequest)
  }
*/










}
