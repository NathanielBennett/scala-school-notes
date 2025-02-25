package com.guardian.gcp

import scala.io.Source
import scala.util.Try

object SegmentParser extends App {

  val segmentMatcher = """^.*segment\s(\S+).*$""".r
  private def s3Segs(fileName: String): List[String] = Source.fromResource(fileName).getLines().toList
    .filter{ s => s.contains("INFO")}
    .flatMap { line =>
      Try {
        val segmentMatcher(segmentId) = line
        segmentId
      }.toOption
    }




  val s3Segments = s3Segs("segment.txt")
  val s3SegmentsTwp = s3Segs("segments_two.txt")
  val runSparkSegs = s3Segs("runspark_segments.txt")

  val outses = Source.fromResource("segment-reader.txt").getLines().toList
  val outsesWed = Source.fromResource("segment-reader-wed.txt").getLines().toList


  val brazeClientSegs = outses.takeWhile(s => !s.isEmpty)
  val maoSegs = outses.reverse.takeWhile(s => !s.isEmpty)

  val brazeClientSegsTwo = outsesWed.takeWhile(s => !s.isEmpty)
  val mooSegsTwo = outsesWed.reverse.takeWhile(s => !s.isEmpty)



  val rawSegs = Source.fromResource("raw_segment.txt").getLines().toList


  println(s3Segments.size)
  println(s3SegmentsTwp.size)
  println(brazeClientSegs.size)
  println(maoSegs.size)
  println(brazeClientSegsTwo.size)
  println(mooSegsTwo.size)
  println(runSparkSegs.size)
  println(rawSegs.size)

  println(brazeClientSegs.toSet == maoSegs.toSet)
  println(brazeClientSegs.toSet == runSparkSegs.toSet)
  println(brazeClientSegs.toSet == rawSegs.toSet)
  println(brazeClientSegsTwo.toSet == rawSegs.toSet)
  println(mooSegsTwo.toSet == rawSegs.toSet)

  val missing = s3Segments.filter{ s => !brazeClientSegs.contains(s) }
  val missingTwo = s3SegmentsTwp.filter{ s => !brazeClientSegs.contains(s) }

  println(missing)
  println(missingTwo)

}
