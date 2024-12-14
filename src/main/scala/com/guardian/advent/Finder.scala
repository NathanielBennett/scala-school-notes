package com.guardian.advent

import requests.Response

object Finder extends App {

  val params = Map(
    "method" -> "address_person",
    "api_key" -> "df81fd503ba33e5d256caf5ed2862d52" ,
    "postcode" -> "LE2 1WN",
    "output" -> "json"
      )

  val r: Response = requests.get("https://api.t2a.io/rest/", params = params)
  println(r.text)
}

