package com.wheaties.squealer.db

case class Column(name: String, typeOf: DataType, default: Option[String], comment: Option[String], colType: ColumnType){
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

case class Database(name: String, tables: List[Table])
case class Table(name: String, comment: Option[String], columns: List[Column])