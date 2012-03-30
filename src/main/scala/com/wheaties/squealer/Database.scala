package com.wheaties.squealer

sealed trait Column{
  val name: String
  val typeOf: String
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

case class ColumnDef(name: String, typeOf: String, default: Option[String]) extends Column
case class NullableColumnDef(name: String, typeOf: String) extends Column
case class PrimaryKeyDef(name: String, typeOf: String, default: Option[String]) extends Column
case class NullablePrimaryKey(name: String, typeOf: String) extends Column

case class Database(name: String, tables: List[Table])
case class Table(name: String, columns: List[Column]){
  val hasPrimaryKey ={
    columns.exists(_.isInstanceOf[PrimaryKeyDef]) || columns.exists(_.isInstanceOf[NullablePrimaryKey])
  }
  def columnCount = columns.size
}
