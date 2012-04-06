package com.wheaties.squealer

import java.sql.Types._
import java.sql.{DriverManager, DatabaseMetaData}

object ParseDataSource extends ((String,String,String) => Database){
  def apply(url: String, account: String, password: String):Database = {
    val connection = DriverManager.getConnection(url, account, password)
    try{
      val tables = parseTables(connection.getMetaData)

      Database(url, tables)
    }
    finally{
      connection.close()
    }
  }

  protected def parseTables(meta: DatabaseMetaData):List[Table] ={
    val tables = meta.getTables(null, null, null, null)
    val tableBuilder = List.newBuilder[Table]
    while(tables.next()){
      val tableName = tables.getString("TABLE_NAME")
      tableBuilder += Table( tableName, parseColumns(meta, tableName))
    }

    tableBuilder.result()
  }

  protected def parsePrimaryKeys(meta: DatabaseMetaData, tableName: String):Set[String] ={
    val keysMeta = meta.getPrimaryKeys(null, null, tableName)
    val keysBuilder = Set.newBuilder[String]
    while(keysMeta.next()){
      keysBuilder += keysMeta.getString("COLUMN_NAME")
    }

    keysBuilder.result()
  }

  //TODO: there's a lot of D-R-Y violations.
  protected def parseColumns(meta: DatabaseMetaData, tableName: String):List[Column] ={
    val keys = parsePrimaryKeys(meta, tableName)

    val columns = meta.getColumns(null, null, tableName, null)
    val columnBuilder = List.newBuilder[Column]
    while(columns.next()){
      val nullable = DatabaseMetaData.columnNoNulls != columns.getInt("NULLABLE")
      val name = columns.getString("COLUMN_NAME")
      val default = Option(columns.getString("COLUMN_DEF"))

      def create(typeOf: String) = typeOf match{
        case _ if nullable && keys.contains(name) => NullablePrimaryKey(name, typeOf)
        case _ if nullable => NullableColumnDef(name, typeOf)
        case _ if keys.contains(name) => PrimaryKeyDef(name, typeOf, default)
        case _ => ColumnDef(name, typeOf, default)
      }

      //following Oracle mapping guide: http://docs.oracle.com/javase/1.5.0/docs/guide/jdbc/getstart/mapping.html
      val column = columns.getInt("DATA_TYPE") match{
        case BIGINT => create("Long")
        case DOUBLE | FLOAT => create("Double")
        case INTEGER => create("Int")
        case NUMERIC | DECIMAL if nullable && keys.contains(name) =>
          new NullablePrimaryKey(name, "BigDecimal") with WithScale{
            override val scale = columns.getInt("COLUMN_SIZE")
            override val precision = columns.getInt("DECIMAL_DIGITS")
          }
        case NUMERIC | DECIMAL if nullable =>
          new NullableColumnDef(name, "BigDecimal") with WithScale{
            override val scale = columns.getInt("COLUMN_SIZE")
            override val precision = columns.getInt("DECIMAL_DIGITS")
          }
        case NUMERIC | DECIMAL if keys.contains(name) =>
          new PrimaryKeyDef(name, "BigDecimal", default) with WithScale{
            override val scale = columns.getInt("COLUMN_SIZE")
            override val precision = columns.getInt("DECIMAL_DIGITS")
          }
        case NUMERIC | DECIMAL => new ColumnDef(name, "BigDecimal", default) with WithScale{
          override val scale = columns.getInt("COLUMN_SIZE")
          override val precision = columns.getInt("DECIMAL_DIGITS")
        }
        case REAL => create("Float")
        case BIT => create("Boolean")
        case SMALLINT | TINYINT => create("Short")
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR if nullable && keys.contains(name) =>
          new NullablePrimaryKey(name, "String") with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR if nullable =>
          new NullableColumnDef(name, "String") with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR if keys.contains(name) =>
          new PrimaryKeyDef(name, "String", default) with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case CHAR | LONGVARCHAR | VARCHAR | LONGNVARCHAR | NCHAR =>
          new ColumnDef(name, "String", default) with WithLength{
            override val length = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY if nullable && keys.contains(name) =>
          new NullablePrimaryKey(name, "Array[Byte]") with WithSize{
            override val size = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY if nullable =>
          new NullableColumnDef(name, "Array[Byte]") with WithSize{
            override val size = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY if keys.contains(name) =>
          new PrimaryKeyDef(name, "Array[Byte]", default) with WithSize{
            override val size = columns.getInt("COLUMN_SIZE")
          }
        case BINARY | VARBINARY | LONGVARBINARY => new ColumnDef(name, "Array[Byte]", default) with WithSize{
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
