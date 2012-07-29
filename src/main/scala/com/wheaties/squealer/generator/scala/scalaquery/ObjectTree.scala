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
import com.wheaties.squealer.db._

/**
 * Creates a ScalaQuery object representing a table in the database.
 */
object ObjectTree extends ((String,List[Column],Formato) => Tree){

  def apply(name: String, columns: List[Column], formato: Formato)={
    declaration(name, columns, formato) := BLOCK{
      projection(columns, formato) :: columns.map(member(_, formato))
    }
  }

  protected[scalaquery] def declaration(name: String, columns: List[Column], formato: Formato) ={
    val types = TYPE_TUPLE{ columns.map(_.typeOf.name) }
    val parent = RootClass.newClass("BasicTable") TYPE_OF(types)

    OBJECTDEF(formato.tableName(name)) withParents(parent APPLY LIT(name))
  }

  protected[scalaquery] def member(column: Column, formato: Formato)={
    val args = column match{
      case Column(_, typeOf, Some(default), _, NullablePrimaryKey) =>
        val value = makeDefaultValue(typeOf, default)
        val defaultDef = REF("O") INFIX("Default") APPLY(SOME(value))
        val keyDef = REF("O") POSTFIX("PrimaryKey")
        keyDef :: defaultDef :: Nil
      case Column(_, typeOf, Some(default), _, NullableColumn) =>
        val value = makeDefaultValue(typeOf, default)
        val defaultDef = REF("O") INFIX("Default") APPLY(SOME(value))
        defaultDef :: Nil
      case Column(_, typeOf, Some(default), _, _) =>
        val value = makeDefaultValue(typeOf, default)
        val defaultDef = REF("O") INFIX("Default") APPLY(value)
        val notNull = REF("O") POSTFIX("NotNull")
        notNull :: defaultDef :: Nil
      case Column(_, _, _, _, NullablePrimaryKey) =>
        val keyDef = REF("O") POSTFIX("PrimaryKey")
        keyDef :: Nil
      case Column(_, _, _, _, PrimaryKey) =>
        val keyDef = REF("O") POSTFIX("PrimaryKey")
        val notNull = REF("O") POSTFIX("NotNull")
        keyDef :: notNull :: Nil
      case Column(_, _, _, _, NullableColumn) => Nil
      case _ =>
        val notNull = REF("O") POSTFIX("NotNull")
        notNull :: Nil
    }

    DEF(formato.columnName(column.name)) := REF("column") APPLYTYPE(column.typeOf.name) APPLY{ LIT(column.name) :: args }
  }

  protected[scalaquery] def makeDefaultValue(typeOf: DataType, default: String) = typeOf match{
    case StringType => LIT(default)
    case FloatType if default.last.toLower != 'f' => REF(default + "f")
    case _ => REF(default)
  }

  protected[scalaquery] def projection(columns: List[Column], formato: Formato) ={
    val tilde = columns.map(col => formato.columnName(col.name)) match{
      case List(single) => REF(single)
      case multi => INFIX_CHAIN("~", multi.map(REF(_)))
    }

    DEF("*") := tilde
  }
  protected[scalaquery] def application(x: Tree, y: Tree):Tree = y INFIX("~") APPLY x
}