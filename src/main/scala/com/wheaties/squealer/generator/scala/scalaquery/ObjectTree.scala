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

package com.wheaties.squealer.generator.scala.scalaquery

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.Formato
import com.wheaties.squealer.db.{StringType, NullablePrimaryKey, NullableColumn, Column}

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
      case Column(_, typeOf, Some(default), _, NullableColumn | NullablePrimaryKey) =>
        val value = if(typeOf == StringType) LIT(default) else REF(default)
        val defaultDef = LIT(0) INFIX("Default") APPLY(SOME(value))
        defaultDef :: Nil
      case Column(_, typeOf, Some(default), _, _) =>
        val notNull = LIT(0) POSTFIX("NotNull")
        val value = if(typeOf == StringType) LIT(default) else REF(default)
        val defaultDef = LIT(0) INFIX("Default") APPLY(value)
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
      case multi => INFIX_CHAIN("~", multi.map(REF(_)))
    }

    DEF("*") := tilde
  }
  protected[scalaquery] def application(x: Tree, y: Tree):Tree = y INFIX("~") APPLY x
}