package com.wheaties.squealer

import java.sql.Types._
import java.sql.{Connection, DriverManager, DatabaseMetaData}

object ParseDataSource extends ((String,String,String) => Database){
  def apply(url: String, account: String, password: String):Database ={
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

      def create(typeOf: String) = Column(name, typeOf, default, comment, columnType)

      //following Oracle mapping guide: http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html
      val column = columns.getInt("DATA_TYPE") match{
        case BIGINT => create("Long")
        case DOUBLE | FLOAT => create("Double")
        case INTEGER => create("Int")
        case NUMERIC | DECIMAL if nullable && keys.contains(name) =>
          new Column(name, "BigDecimal", default, comment, columnType) with WithScale{
            override val scale = columns.getInt("COLUMN_SIZE")
            override val precision = columns.getInt("DECIMAL_DIGITS")
          }
        case REAL => create("Float")
        case BIT => create("Boolean")
        case SMALLINT | TINYINT => create("Short")
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR if nullable && keys.contains(name) =>
          new Column(name, "String", default, comment, columnType) with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY if nullable && keys.contains(name) =>
          new Column(name, "Array[Byte]", default, comment, columnType) with WithSize{
            override val size = columns.getInt("COLUMN_SIZE")
          }
        case DATE => create("Date")
        case TIME => create("Time")
        case TIMESTAMP => create("Timestamp")
        case CLOB => create("Clob")
        case BLOB => create("Blob")
        case ARRAY => create("Array")
        case _ => create("Object")
      }
      columnBuilder += column
    }

    columnBuilder.result()
  }
}