package com.guardian.gcp

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.common.collect.ImmutableList

import java.io.FileInputStream
import java.util

object CredsBuilder extends App {
  val SERVICE_ACCOUNT_FILE = "/home/nathaniel/.gcp/group-monitor.json"

  val l: ImmutableList[String] = ImmutableList.of(
    "https://www.googleapis.com/auth/cloud-identity.groups",
    "https://www.googleapis.com/auth/admin.directory.group",
    "https://www.googleapis.com/auth/admin.directory.group.member",
    "https://www.googleapis.com/auth/apps.groups.settings"
  )

  println("Hello")
  val accessToken = getAccessTokenFromJsonKey()
  println(s"Access token: ${accessToken}")


  private def getAccessTokenFromJsonKey(): String = {
    val transport = new NetHttpTransport()
    val credential = GoogleCredential.fromStream(
      new FileInputStream(SERVICE_ACCOUNT_FILE),
      transport,
      GsonFactory.getDefaultInstance
    )

    println(s"ServiceAccountiD: ${credential.getServiceAccountId}")
    val jsonFactory = GsonFactory.getDefaultInstance

    val builtCredentials = new GoogleCredential.Builder()
      .setServiceAccountPrivateKey(credential.getServiceAccountPrivateKey)
      .setServiceAccountPrivateKeyId(credential.getServiceAccountPrivateKeyId)
      .setServiceAccountId(credential.getServiceAccountId)
      .setTransport(transport)
      .setJsonFactory(GsonFactory.getDefaultInstance)
      .setServiceAccountScopes(l)
      .setClock(credential.getClock)
      .build()

    if (!builtCredentials.refreshToken()) {
      throw new Exception("Access token")
    }
    builtCredentials.getAccessToken
  }

}

