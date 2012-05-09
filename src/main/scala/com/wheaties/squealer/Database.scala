package com.wheaties.squealer

case class Column(name: String, typeOf: String, default: Option[String], comment: Option[String], colType: ColumnType){
  def size: Int = 0
  def precision: Int = 0
  def scale: Int = 0
  def length: Int = 0
}
trait WithSize{
  def size: Int
}
trait WithScale{
  def scale: Int
  def precision: Int
}
trait WithLength{
  def length: Int
}

sealed trait ColumnType
case object ColumnDef extends ColumnType
case object NullableColumn extends ColumnType
case object PrimaryKey extends ColumnType
case object NullablePrimaryKey extends ColumnType

//case class ColumnDef(name: String, typeOf: String, default: Option[String], comment: Option[String]) extends Column
//case class NullableColumnDef(name: String, typeOf: String, comment: Option[String]) extends Column
//case class PrimaryKeyDef(name: String, typeOf: String, default: Option[String], comment: Option[String]) extends Column
//case class NullablePrimaryKey(name: String, typeOf: String, comment: Option[String]) extends Column

case class Database(name: String, tables: List[Table])
case class Table(name: String, comment: Option[String], columns: List[Column])