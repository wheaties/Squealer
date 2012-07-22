package com.wheaties.squealer.generator

import com.wheaties.squealer.db.{Database, Table}

trait LibraryGenerator {
  def writeTable(table: Table, group: String, formato: Formato):String
  def writeDatabase(db: Database, group: String, formato: Formato):String
}