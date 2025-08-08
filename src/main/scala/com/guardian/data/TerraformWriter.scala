package com.guardian.data

import cats.syntax.either._
import io.circe.yaml._
import io.circe.yaml.syntax._

case class TableDefin(primary_definition_source: String, view: Option[Boolean])

object TerraformWriter extends App {

}
