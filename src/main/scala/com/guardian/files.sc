val gcs = "segment-export-flattened/date=2025-01-14/00290937-c037-4d82-9874-34f51977be32:833042b9-de14-419b-b1f9-2ec63a86e1a3-1736813469:0008ef2835610e9595291bc0b7f20568.gz"
val s3Key = "segment-export/00290937-c037-4d82-9874-34f51977be32/2025-01-14/833042b9-de14-419b-b1f9-2ec63a86e1a3-1736813469/0008ef2835610e9595291bc0b7f20568.gz"

val l = gcs.split("/").toList

for {
  filename <- l.lastOption
  splitList = filename.split(":").toList
  segmentId <- splitList.lift(0)
  transactionId <- splitList.lift(1)
} yield (segmentId, transactionId)

