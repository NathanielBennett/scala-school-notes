package com.guardian.gcp

import com.google.cloud.bigquery.InsertAllRequest.RowToInsert
import com.google.cloud.bigquery.{BigQueryOptions, InsertAllRequest, LoadJobConfiguration, QueryJobConfiguration, TableId}
import com.google.common.collect.ImmutableList
import jdk.nashorn.internal.ir.annotations.Immutable
import org.eclipse.collections.impl.factory.Multimaps.ImmutableMultimaps.ImmutableListMultimapFactory

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._

object TableChecker extends App {

  val bigQuery = BigQueryOptions.newBuilder().build().getService
  val tableId = TableId.of("datatech-platform-code", "staging", "braze_current_segment_ids")
 // val tableId = TableId.of("datatech-platform-code.staging".braze_current_segment_ids")

  println(tableId.toString)
  println(tableId.getProject)

  val segmentIds = List("segment_1", "segment_2", "segment_3")
    .map{ segmmentId => RowToInsert.of( Map("segment_id" -> segmmentId, "export_date" -> LocalDate.now().plusDays(1).format(DateTimeFormatter.ISO_DATE)).asJava) }


  val insertAllRequest = InsertAllRequest.newBuilder(tableId)
    .setRows(segmentIds.asJava)
    .build()


  val insertAllResponse = bigQuery.insertAll(insertAllRequest)


  if (insertAllResponse.hasErrors) {
    insertAllResponse.getInsertErrors.entrySet.asScala.foreach{
      e => println(e.toString)
    }
  }
  println("Done")

  val query = "select * from datatech-platform-code.staging.braze_current_segment_ids"
  val queryCofig = QueryJobConfiguration.newBuilder(query).build()
  val d =  bigQuery.query(queryCofig)
  val segs = d.iterateAll().asScala.map( s => s.get("segment_id").getStringValue ).toList
  println(segs)

}
