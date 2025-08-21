package com.guardian.gcp.backfill

import com.google.storagetransfer.v1.proto.StorageTransferServiceClient
import com.google.storagetransfer.v1.proto.TransferProto.{CreateTransferJobRequest, RunTransferJobRequest}
import com.google.storagetransfer.v1.proto.TransferTypes.{AwsAccessKey, AwsS3Data, MetadataOptions, TransferJob, TransferOptions, TransferSpec}

import scala.util.{Failure, Success, Try}

object ConsentsBackfill extends App {

  def getTransferServer: Option[StorageTransferServiceClient] = Try {
    StorageTransferServiceClient.create
  }.recoverWith{
    case throwable: Throwable => println(s"Could not create client ${throwable.getMessage}")
      Failure(throwable)
  }.toOption

  getTransferServer.foreach {
    transferService =>

     val awsDataSourrce = AwsS3Data.newBuilder()
       .setBucketName("ophan-clean-consents")
       .setPath("export_date=2018-08-07/")

       .setAwsAccessKey(
         AwsAccessKey.newBuilder()
           .setAccessKeyId("xx")
           .setSecretAccessKey("zx")
       )
       .build()

     val transferSpec = TransferSpec.newBuilder()
        .setAwsS3DataSource(awsDataSourrce)
        .build()

      val transferJob = TransferJob.newBuilder()
        .setProjectId("datatech-platform-prod")
        .setTransferSpec(transferSpec)
        .setStatus(TransferJob.Status.ENABLED)
        .build()

      val transferResoonse = transferService.createTransferJob(
        CreateTransferJobRequest.newBuilder()
          .setTransferJob(transferJob)
          .build()
      )

      transferService.runTransferJobAsync(
        RunTransferJobRequest.newBuilder()
          .setProjectId("datalake-platform-prod")
          .setJobName(transferResoonse.getName)
          .build()
      ).get()

      println("Done")
  }
}
