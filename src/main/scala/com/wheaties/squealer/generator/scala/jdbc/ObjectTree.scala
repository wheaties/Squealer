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

package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import scala.annotation._

object ObjectTree extends ((String,List[Column]) => Tree){
  def apply(name: String, params: List[Column]) ={
    val raw = DEF("apply") withParams(PARAM("result", "ResultSet")) := {
      NEW(name, extract(params, nakedGet): _*)
    }
    val namespaced = DEF("apply") withParams(PARAM("result", "ResultSet"), PARAM("ns", "String")) := {
      NEW(name, extract(params, namespacedGet): _*)
    }

    OBJECTDEF(name) := BLOCK{ raw :: namespaced :: Nil }
  }

  protected[jdbc] def extract(params: List[Column], function: (String,DataType)=> Apply) ={
    @tailrec def make(remainder: List[Column], acc: List[Tree]):List[Tree] = remainder match{
      case Column(name, typeOf, _, _, ColumnDef) :: xs => make(xs, function(name, typeOf) :: acc)
      case Column(name, typeOf, _, _, PrimaryKey) :: xs => make(xs, function(name, typeOf) :: acc)
      case Column(name, typeOf, _, _, NullableColumn) :: xs =>
        val value = REF("Option") APPLY(function(name, typeOf))
        make(xs, value :: acc)
      case Column(name, typeOf, _, _, NullablePrimaryKey) :: xs =>
        val value = REF("Option") APPLY(function(name, typeOf))
        make(xs, value :: acc)
      case Nil => acc.reverse
    }

    make(params, Nil)
  }

  protected[jdbc] def nakedGet(name: String, typeOf: DataType):Apply = if(typeOf == BinaryType){
    REF("result") DOT("getBytes") APPLY REF(name)
  }
  else{
    REF("result") DOT("get" + typeOf.name) APPLY REF(name)
  }

  protected[jdbc] def namespacedGet(name: String, typeOf: DataType):Apply = if(typeOf == BinaryType){
    REF("result") DOT("getBytes") APPLY{ REF("ns") INFIX("+") APPLY REF(name) }
  }
  else{
    REF("result") DOT("get" + typeOf.name) APPLY{ REF("ns") INFIX("+") APPLY REF(name) }
  }
}
