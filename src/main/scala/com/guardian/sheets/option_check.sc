val s3 = """^ophan-raw-([A-Za-z0-9\-\.]+)$""".r

def googleStorageBucket(stage: String, s3Bucket: String) = s3Bucket match {
  case s3(slug) => s"gs://gu-datatech-raw-${slug}-${stage}"
}


def replace(raw: String, stage: String): String =
  s"gs://${raw.replace("ophan","gu-datatech")}-${stage}"

val a = googleStorageBucket("code", "ophan-raw-dfp-lineitem")
val b = replace("ophan-raw-dfp-lineitem", "code")