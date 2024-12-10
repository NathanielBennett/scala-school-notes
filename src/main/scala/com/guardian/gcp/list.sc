case class IdentityId(id: String)

val idList = List("123432", "1232", "542334").map(IdentityId(_))

case class SegmentRecord(external_id: IdentityId, segmentIds: List[String])

val segmentRecords = SegmentRecord(IdentityId("1232"), List("oxxxne, two")) == SegmentRecord(IdentityId("1232"), List("one, two"))