package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import scala.annotation._

object ObjectTree extends ((String,List[Column]) => Tree){
  def apply(name: String, params: List[Column]) ={
    OBJECTDEF(name) := BLOCK{
      DEF("apply") withParams(PARAM("result", "ResultSet")) := {
        NEW(name, extract(params): _*)
      }
    }
  }

  protected[squealer] def extract(params: List[Column]) ={
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

  //Don't hate me 'cause I'm using stringly types here
  protected def function(name: String, typeOf: DataType) = if(typeOf == BinaryType){
    REF("result") DOT("getBytes") APPLY REF(name)
  }
  else{
    REF("result") DOT("get" + typeOf.name) APPLY REF(name)
  }
}
