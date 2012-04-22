package com.wheaties.squealer

//TODO: add in a regex based formato impl

/**
 * Formato: takes a table and gives you back a cleanly, squeaky table
 */
trait Formato extends (Table => Table){

  def apply(table: Table)={
    val Table(name, comment, columns) = table

    Table(format(name).capitalize, comment, columns.map(formatColumns))
  }

  //TODO: this is how I should be reducing D-R-Y in the code
  def formatColumns(column: Column)= column match{
    case col @ ColumnDef(name, typeOf, default, comment) => new ColumnDef(format(name), typeOf, default, comment){
      override def size: Int = col.size
      override def precision: Int = col.precision
      override def scale: Int = col.scale
      override def length: Int = col.length
    }
    case col @ NullableColumnDef(name, typeOf, comment) => new NullableColumnDef(name, typeOf, comment){
      override def size: Int = col.size
      override def precision: Int = col.precision
      override def scale: Int = col.scale
      override def length: Int = col.length
    }
    case col @ PrimaryKeyDef(name, typeOf, default, comment) => new PrimaryKeyDef(name, typeOf, default, comment){
      override def size: Int = col.size
      override def precision: Int = col.precision
      override def scale: Int = col.scale
      override def length: Int = col.length
    }
    case col @ NullablePrimaryKey(name, typeOf, comment) => new NullablePrimaryKey(name, typeOf, comment){
      override def size: Int = col.size
      override def precision: Int = col.precision
      override def scale: Int = col.scale
      override def length: Int = col.length
    }
  }

  protected def format(name: String):String
}

object CamelCase extends Formato{
  /**
   *  Change "HelloWorld" to "helloWorld"
   *  Change "Hello_World" to "helloWorld"
   */
  def format(in: String): String = in.split("[_/s]").toList match{
    case word :: Nil if word.nonEmpty => word.charAt(0).toLower + word.drop(1)
    case firstWord :: rest => firstWord.toLowerCase + rest.map(_.capitalize).mkString
    case _ => ""
  }
}