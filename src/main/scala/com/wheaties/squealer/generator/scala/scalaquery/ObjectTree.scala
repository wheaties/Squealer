package com.wheaties.squealer.generator.scala.scalaquery

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.db.{NullablePrimaryKey, NullableColumn, Column}
import com.wheaties.squealer.generator.Formato

/**
 * Creates a ScalaQuery object representing a table in the database.
 */
object ObjectTree extends ((String,List[Column],Formato) => Tree){

  def apply(name: String, columns: List[Column], formato: Formato)={
    declaration(name, columns, formato) := BLOCK{
      projection(columns) :: columns.map(member(_, formato))
    }
  }

  protected[scalaquery] def declaration(name: String, columns: List[Column], formato: Formato) ={
    val types = TYPE_TUPLE{ columns.map(_.typeOf.name) }
    val parent = RootClass.newClass("BasicTable") TYPE_OF(types)

    OBJECTDEF(formato.tableName(name)) withParents(parent APPLY LIT(name))
  }

  protected[scalaquery] def member(column: Column, formato: Formato)={
    val args = column match{
      case Column(_, _, Some(default), _, NullableColumn | NullablePrimaryKey) =>
        val defaultDef = LIT(0) INFIX("Default") APPLY(SOME(REF(default)))
        defaultDef :: Nil
      case Column(_, _, Some(default), _, _) =>
        val notNull = LIT(0) POSTFIX("NotNull")
        val defaultDef = LIT(0) INFIX("Default") APPLY(REF(default))
        notNull :: defaultDef :: Nil
      case Column(_, _, _, _, NullableColumn | NullablePrimaryKey) => Nil
      case _ =>
        val notNull = LIT(0) POSTFIX("NotNull")
        notNull :: Nil
    }

    DEF(formato.columnName(column.name)) := REF("column") APPLYTYPE(column.typeOf.name) APPLY{ LIT(column.name) :: args }
  }

  protected[scalaquery] def projection(columns: List[Column]) ={
    val tilde = columns.map(_.name) match{
      case List(single) => REF(single)
      case multi => multi.map(REF(_)).reduce( application )
    }

    DEF("*") := tilde
  }
  protected[scalaquery] def application(x: Tree, y: Tree):Tree = y INFIX("~") APPLY x
}