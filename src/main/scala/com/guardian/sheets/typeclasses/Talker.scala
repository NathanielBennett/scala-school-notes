package com.guardian.sheets.typeclasses

trait Talker[A] {
  def chat(a: A): String
}
