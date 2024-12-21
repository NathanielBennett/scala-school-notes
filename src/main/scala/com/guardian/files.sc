val allFields = Set(
  "braze_uuid",
  "registration_type",
  "social_links",
  "last_active_date",
  "receive_marketing",
  "email_validated",
  "primary_email_address",
  "telephone_country_code",
  "telephone_local_number",
  "country",
  "address1",
  "address2",
  "town",
  "county_or_state",
  "postcode",
  "registration_location",
  "first_name",
  "second_name",
  "title",
  "has_password",
  "okta_id",
  "is_jobs_user",
  "registration_platform",
  "inferred_country",
  "inferred_marketing_region",
  "meta_created_at",
  "account_created",
  "meta_produced_at"
)

val allowed = Set(
  "account_created",
  "id",
  "registration_location",
  "braze_uuid",
  "account_created",
  "last_active_date",
  "email_validated",
  "registration_platform",
  "has_password"
)


allFields.diff(allowed).toList.sorted.foreach(println)