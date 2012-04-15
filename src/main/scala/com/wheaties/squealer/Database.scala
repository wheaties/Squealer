package com.wheaties.squealer

sealed trait Column{
  def name: String
  def typeOf: String
  def comment: Option[String]
  def size: Int = 0
  def precision: Int = 0
  def scale: Int = 0
  def length: Int = 0
}
trait WithSize{
  val size: Int
}
trait WithScale{
  val scale: Int
  val precision: Int
}
trait WithLength{
  val length: Int
}

case class ColumnDef(name: String, typeOf: String, default: Option[String], comment: Option[String]) extends Column
case class NullableColumnDef(name: String, typeOf: String, comment: Option[String]) extends Column
case class PrimaryKeyDef(name: String, typeOf: String, default: Option[String], comment: Option[String]) extends Column
case class NullablePrimaryKey(name: String, typeOf: String, comment: Option[String]) extends Column

case class Database(name: String, tables: List[Table])
case class Table(name: String, comment: Option[String], columns: List[Column])
