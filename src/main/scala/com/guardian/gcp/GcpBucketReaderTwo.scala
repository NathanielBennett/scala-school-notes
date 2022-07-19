package com.guardian.gcp

import com.google.api.client.googleapis.apache.v2.GoogleApacheHttpTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.apache.v2.ApacheHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.storage.{Storage, StorageScopes}
import com.google.api.services.storage.model.StorageObject

import java.io.{ByteArrayInputStream, InputStream}
import java.util.{Collections, List => JavaList}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Try}



object GcpBucketReaderTwo extends App{

  lazy val httpTransport: ApacheHttpTransport = GoogleApacheHttpTransport.newTrustedTransport
  lazy val jsonFactory: GsonFactory = GsonFactory.getDefaultInstance
  private val bucketName = "europe-west2-datatech-airfl-fdf1e4b5-bucket"

  authorize().map{
    credentials =>
      println("Got credentials")
      val storage = new Storage.Builder(httpTransport, jsonFactory, credentials ).build()
      val gcsQuery = storage
        .objects()
        .list("europe-west2-datatech-airfl-fdf1e4b5-bucket")

      getResults(gcsQuery).filter(c => c.getName.endsWith("py")).foreach(
        c => println(s"**${c.getName}**")
      )
  }



  @tailrec
  def getResults(query: Storage#Objects#List, foundSoFar: List[StorageObject] = List.empty): List[StorageObject] = {

    val results = query.execute

    Option(results.getNextPageToken) match {
      case None => foundSoFar
      case Some(nextPageToken) => getResults(query.setPageToken(nextPageToken), foundSoFar)
    }
  }



  private def authorize():  Try[GoogleCredential] = {

    val serviceAccountScopes = Collections.singleton(StorageScopes.DEVSTORAGE_FULL_CONTROL)

    println("Authing")
    readResourceWithNiceError("secret.json").map {
      is =>
        val creds = GoogleCredential.fromStream(is, httpTransport, jsonFactory).createScoped(
          serviceAccountScopes
        )

        new GoogleCredential.Builder()
          .setTransport(creds.getTransport)
          .setJsonFactory(jsonFactory)
          .setServiceAccountScopes(creds.getServiceAccountScopes)
          .setServiceAccountPrivateKey(creds.getServiceAccountPrivateKey)
          .build()
    }
  }

  private def readInputStrem(inputStreamName: String): Try[InputStream] = Try {
    getClass.getClassLoader.getResourceAsStream(inputStreamName)
  }

  def readResourceWithNiceError(resourcePath: String): Try[InputStream] =
    Try{
      val src = Source.fromResource(resourcePath).getLines.mkString
      new ByteArrayInputStream(src.getBytes)
    }
    .recover{
      val e = new Exception(resourcePath)
      println(s"Ex ${e.getCause} ${e.getMessage}")
      throw e
    }


}
