package com.wheaties.squealer.generator.scala.scalaquery

//import org.scalaquery.ql.basic.BasicTable
//import org.scalaquery.ql.TypeMapper._
//import org.scalaquery.ql._

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.db.{NullablePrimaryKey, NullableColumn, Column}


object ScalaQueryTree extends ((String,String,List[Column]) => Tree){

  def apply(name: String, columns: List[Column])={
    declaration(name, tableName, columns) := BLOCK{
      projection(columns) :: columns.map(member)
    }
  }

  protected[scalaquery] def declaration(name: String, tableName: String, columns: List[Column]) ={
    val types = TYPE_TUPLE{ columns.map(_.typeOf.name) }
    val parent = RootClass.newClass("BasicTable") TYPE_OF(types)

    OBJECTDEF(name) withParents(parent APPLY LIT(tableName))
  }

  //TODO: totally breaking type safety here 'cause I don't know how to get what I want.
  protected[scalaquery] def member(column: Column)={
    val args = column match{
      case Column(_, _, Some(default), _, NullableColumn | NullablePrimaryKey) =>
        val defaultDef = REF("0 Default Some(%s)".format(default))//LIT(0) REF("Default") SOME(REF(default))
        defaultDef :: Nil
      case Column(_, _, Some(default), _, _) =>
        val notNull = REF("0 NotNull")//LIT(0) REF("NotNull")
        val defaultDef = REF("0 Default %s".format(default))//LIT(0)  REF("Default") REF(default)
        notNull :: defaultDef :: Nil
      case Column(_, _, _, _, NullableColumn | NullablePrimaryKey) => Nil
      case _ =>
        val notNull = REF("0 NotNull")//LIT(0) REF("NotNull")
        notNull :: Nil
    }

    DEF(column.name) := REF("column") APPLYTYPE(column.typeOf.name) APPLY{ LIT(column.name) :: args }
  }

  protected[scalaquery] def projection(columns: List[Column]) ={
    val refs = columns.map(col => REF(col.name))
    DEF("*") := refs.reduceLeft{ application }
  }
  protected[scalaquery] def application(x: Tree, y: Ident):Tree = (y DOT "~") APPLY x
}