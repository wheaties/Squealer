/*
Copyright (c) 2012 Owein Reese

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.wheaties.squealer.db

//TODO: factor out these Scala specific data type mappings. This a first step.
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
case object UnknownType extends DataType("?"){
  override def isString = true
  override def isNumeric = true
  override def isDate = true
}