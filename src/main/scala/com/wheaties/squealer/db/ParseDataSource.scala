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

import java.sql.Types._
import java.sql.{Connection, DriverManager, DatabaseMetaData}

object ParseDataSource extends ((String,String,String,String) => Database){
  def apply(url: String, account: String, password: String, driver: String):Database ={
    Class.forName(driver)
    val connection = DriverManager.getConnection(url, account, password)

    parseDatabase(url, connection)
  }

  protected[squealer] def parseDatabase(url: String, connection: Connection) = try{
    val tables = parseTables(connection.getMetaData)

    Database(url, tables)
  }
  finally{
    connection.close()
  }

  //TODO: perhaps also include a description of the indexes on each table
  protected[squealer] def parseTables(meta: DatabaseMetaData):List[Table] ={
    val tables = meta.getTables(null, null, null, null)
    val tableBuilder = List.newBuilder[Table]
    while(tables.next()){
      val tableName = tables.getString("TABLE_NAME")
      val comment = Option(tables.getString("REMARKS"))
      tableBuilder += Table(tableName, comment, parseColumns(meta, tableName))
    }

    tableBuilder.result()
  }

  protected[squealer] def parsePrimaryKeys(meta: DatabaseMetaData, tableName: String):Set[String] ={
    val keysMeta = meta.getPrimaryKeys(null, null, tableName)
    val keysBuilder = Set.newBuilder[String]
    while(keysMeta.next()){
      keysBuilder += keysMeta.getString("COLUMN_NAME")
    }

    keysBuilder.result()
  }

  /**
   * TODO: Columns can have constraints. They can be limited to a set, a range , a condition or computed.
   * Think about how to capture it. Computed columns constraints are generally computed with values from the same row.
   * This is more than likely database specific.
   */
  protected[squealer] def parseColumns(meta: DatabaseMetaData, tableName: String):List[Column] ={
    val keys = parsePrimaryKeys(meta, tableName)

    val columns = meta.getColumns(null, null, tableName, null)
    val columnBuilder = List.newBuilder[Column]
    while(columns.next()){
      val nullable = DatabaseMetaData.columnNoNulls != columns.getInt("NULLABLE")
      val name = columns.getString("COLUMN_NAME")
      val default = Option(columns.getString("COLUMN_DEF"))
      val comment = Option(columns.getString("REMARKS"))
      val columnType = if(keys.contains(name)){
        if(nullable) NullablePrimaryKey else PrimaryKey
      }
      else{
        if(nullable) NullableColumn else ColumnDef
      }

      def create(typeOf: DataType) = Column(name, typeOf, default, comment, columnType)

      val column = columns.getInt("DATA_TYPE") match{
        case BIGINT => create(LongType)
        case DOUBLE | FLOAT => create(DoubleType)
        case INTEGER => create(IntType)
        case NUMERIC | DECIMAL =>
          new Column(name, DecimalType, default, comment, columnType) with WithScale{
            override val scale = columns.getInt("COLUMN_SIZE")
            override val precision = columns.getInt("DECIMAL_DIGITS")
          }
        case REAL => create(FloatType)
        case BIT => create(BooleanType)
        case SMALLINT | TINYINT => create(ShortType)
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR =>
          new Column(name, StringType, default, comment, columnType) with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY =>
          new Column(name, BinaryType, default, comment, columnType) with WithSize{
            override val size = columns.getInt("COLUMN_SIZE")
          }
        case DATE => create(DateType)
        case TIME => create(TimeType)
        case TIMESTAMP => create(TimestampType)
        case CLOB => create(ClobType)
        case BLOB => create(BlobType)
        case ARRAY => create(ArrayType)
        case _ => create(ObjectType)
      }
      columnBuilder += column
    }

    columnBuilder.result()
  }
}