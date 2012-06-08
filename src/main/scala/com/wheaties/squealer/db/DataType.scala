package com.wheaties.squealer.db

//TODO: factor out these Scala specific data type mappings. This a first step to that end.
//following Oracle mapping guide: http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html
abstract class DataType(scalaName: String){
  def isNumeric = false
  def isString = false
  def isDate = false
  def isSortable = isNumeric || isDate
  def name = scalaName //TODO: this the bad Scala part
}
case object DecimalType extends DataType("BigDecimal"){
  override def isNumeric = true
}
case object LongType extends DataType("Long"){
  override def isNumeric = true
}
case object IntType extends DataType("Int"){
  override def isNumeric = true
}
case object DoubleType extends DataType("Double"){
  override def isNumeric = true
}
case object FloatType extends DataType("Float"){
  override def isNumeric = true
}
case object ShortType extends DataType("Short"){
  override def isNumeric = true
}
case object BooleanType extends DataType("Boolean")
case object StringType extends DataType("String"){
  override def isString = true
}
case object DateType extends DataType("Date"){
  override def isDate = true
}
case object TimeType extends DataType("Time"){
  override def isDate = true
}
case object TimestampType extends DataType("Timestamp"){
  override def isDate = true
}
case object BinaryType extends DataType("Array[Byte]")
case object ClobType extends DataType("Clob")
case object BlobType extends DataType("Blob")
case object ArrayType extends DataType("Array")
case object ObjectType extends DataType("Object")
case object UnknownType extends DataType("?")