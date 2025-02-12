import io.circe._
import io.circe.syntax._
import io.circe.Parser
import io.circe.parser._
import io.circe.generic._
import io.circe.jawn.JawnParser

import scala.io.Source
import scala.reflect.io.File
import scala.util.Try


case class MissingExternalIdError(message: String) extends RuntimeException(message)
case class MissingIdentityIdError(message: String) extends RuntimeException(message)



def parseJson(jsonString: String): Either[ParsingFailure, Json] =
  Try{
    Json.fromJsonObject(jsonString.asJson)

}.toEither
    .fold(th => Left(ParsingFailure(th.getMessage, th)), json => Right(json))

//def parseJson(jsonString: String): Either[ParsingFailure, Json] = parser.parse(jsonString)


case class CustomAttributes(identityId: String)


//println(System.getProperty("user.home"))
val raw = Source.fromFile(s"${System.getProperty("user.home")}/braze_raw.txt").getLines.toList

val parsed  = raw.flatMap{ jsonString => parse(jsonString).toOption }
  //.map{ json =>  json.hcursor.downField("custom_attributes").get[Json]("identity_id")  }

val stringParsed = raw.flatMap{ jsonString => parseJson(jsonString).toOption }
  .flatMap { json =>
    json.hcursor.values.toList
  }














