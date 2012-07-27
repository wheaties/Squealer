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

object ConstructorTree extends ((String,List[Column]) => ClassDefStart){
  def apply(name: String, params: List[Column]):ClassDefStart ={
    val start = if(params.size < 23) CASECLASSDEF(name) else CLASSDEF(name)
    start withParams(make(params))
  }

  private def paramStart(params: List[Column]) = if(params.size < 23) caseStart _ else classStart _
  private def caseStart(name: String, typeOf: Type):ValDef = PARAM(name, typeOf)
  private def classStart(name: String, typeOf: Type):ValDef = VAL(name, typeOf)
  private def defaultStart(params: List[Column]) = if(params.size < 23) caseDefault _ else classDefault _
  private def caseDefault(name: String, typeOf: Type, default: String):ValDef = PARAM(name, typeOf) := LIT(default)
  private def classDefault(name: String, typeOf: Type, default: String):ValDef = VAL(name, typeOf) := LIT(default)

  private[squealer] def make(params: List[Column]):List[ValDef] ={
    val start = paramStart(params)
    val withDefault = defaultStart(params)
    params.map{
      _ match{
        case Column(name, typeOf, Some(default), _, ColumnDef | PrimaryKey) => withDefault(name, typeOf.name, default)
        case Column(name, typeOf, _, _, ColumnDef | PrimaryKey) => start(name, typeOf.name)
        case Column(name, typeOf, _, _, NullableColumn | NullablePrimaryKey) => start(name, TYPE_OPTION(typeOf.name))
      }
    }
  }
}

object AssumptionTree extends (List[Column] => List[Tree]){
  def apply(params: List[Column]):List[Tree] = {
    @tailrec def make(remainder: List[Column], acc: List[Tree] = Nil):List[Tree] = remainder match{
      case Column(name, _, _, _, NullableColumn | NullablePrimaryKey) :: xs if check(remainder.head) =>
        val assumeOpt = REF(name) MAP LAMBDA(PARAM("x")) ==> makeAssumption(remainder.head, "x")
        make(xs, assumeOpt :: acc)
      case Column(name, _, _, _, NullablePrimaryKey) :: xs if check(remainder.head) =>
        val assumeOpt = REF(name) MAP LAMBDA(PARAM("x")) ==> makeAssumption(remainder.head, "x")
        make(xs, assumeOpt :: acc)
      case x :: xs if check(x) => make(xs, makeAssumption(x, x.name) :: acc)
      case x :: xs => make(xs, acc)
      case Nil => acc
    }

    make(params)
  }

  private[squealer] def check(column: Column) = column.length > 0 || column.precision > 0 || column.size > 0 || column.scale > 0

  private[squealer] def makeAssumption(column: Column, name: String) = Predef_assume APPLY{
    val (condition, msg) = column match{
      case _ if column.size > 0 => (assumption(name, "size", column.size), message(name, "size", column.size))
      case _ if column.length > 0 => (assumption(name, "length", column.length), message(name, "length", column.length))
      case _ if column.precision > 0 && column.scale > 0 =>
        (assumption(name, "precision", column.precision) AND assumption(name, "scale", column.scale),
         message(name, "precision", column.precision))
      case _ => (assumption(name, "precision", column.precision), message(name, "precision", column.precision))
    }
    condition :: msg :: Nil
  }

  private def assumption(name: String, restrict: String, value: Int) = REF(name) DOT restrict INT_<= LIT(value)
  private def message(name: String, restrict: String, value: Int) =
    LIT(name + "." + restrict +" must be less than " + value.toString)
}

object HashCodeTree extends (List[Column] => Tree){
  def apply(params: List[Column]):Tree ={
    if(params.exists(col => col.colType == PrimaryKey || col.colType == NullablePrimaryKey)){
      DEF("hashCode") withFlags(Flags.OVERRIDE) := defineHashCode(withKeys(params))
    }
    else if(params.length > 22){
      LAZYVAL("hashCode") withFlags(Flags.OVERRIDE) := defineHashCode(withoutKeys(params))
    }
    else{
      EmptyTree
    }
  }

  private[squealer] def defineHashCode(hashed: List[SelectStart]):Tree ={
    LIST(hashed) REDUCELEFT LAMBDA(PARAM(TUPLE(REF("left"), REF("right")))) ==>{
      PAREN(REF("left") INT_* LIT(17)) INFIX("^") APPLY REF("right")
    }
  }

  @tailrec private[squealer] def withKeys(remainder: List[Column], acc: List[SelectStart] = Nil):List[SelectStart] = remainder match{
    case Column(name, _, _ ,_, PrimaryKey) :: xs =>
      val key = REF(name) DOT "hashCode"
      withKeys(xs, key :: acc)
    case Column(name, _, _, _, NullablePrimaryKey) :: xs =>
      val key = REF(name) DOT "hashCode"
      withKeys(xs, key :: acc)
    case x :: xs => withKeys(xs, acc)
    case Nil => acc
  }

  private[squealer] def withoutKeys(params: List[Column]) = params.map(col => REF(col.name) DOT "hashCode")
}

object EqualsTree extends ((String, List[Column]) => Tree){
  def apply(tableName: String, params: List[Column]):Tree ={
    if(params.exists(col => col.colType == PrimaryKey || col.colType == NullablePrimaryKey)){
      defineEquals(makeWithKeys(tableName, params))
    }
    else if(params.size > 22){
      defineEquals(makeWithoutKeys(tableName, params))
    }
    else{
      EmptyTree
    }
  }

  private[squealer] def defineEquals(made: List[CaseDef]) ={
    DEF("equals") withFlags(Flags.OVERRIDE) withParams(PARAM("that", "Any")) := REF("that") MATCH{
      made
    }
  }

  private[squealer] def makeWithKeys(tableName: String, params: List[Column]):List[CaseDef] ={
    (CASE(REF(tableName) UNAPPLY pattern(params)) ==>(withKeys(params))) ::
    (CASE(WILDCARD) ==> FALSE) :: Nil
  }

  @tailrec private[squealer] def withKeys(remainder: List[Column], acc: List[Infix] = Nil):Infix = remainder match{
    case Column(name, _, _, _, PrimaryKey) :: xs =>
      val param = (THIS DOT name) ANY_== (REF("that") DOT name)
      withKeys(xs, param :: acc)
    case Column(name, _, _, _, NullablePrimaryKey) :: xs =>
      val param = (THIS DOT name) ANY_== (REF("that") DOT name)
      withKeys(xs, param :: acc)
    case x :: xs => withKeys(xs, acc)
    case Nil => acc.reduce((left, right) => left AND right)
  }

  private[squealer] def pattern(params: List[Column]):List[Ident] = params map{
    _ match{
        case Column(name, _, _, _, PrimaryKey) => ID(name)
        case Column(name, _, _, _, NullablePrimaryKey) => ID(name)
        case _ => WILDCARD
    }
  }

  private[squealer] def makeWithoutKeys(tableName: String, params: List[Column]):List[CaseDef] ={
      (CASE(ID("x") withType(tableName)) ==>(withoutKeys(params))) :: (CASE(WILDCARD) ==> FALSE) :: Nil
  }

  private[squealer] def withoutKeys(params: List[Column]):Infix ={
    val checks = params.map(col => (THIS DOT col.name) ANY_== (REF("that") DOT col.name) )
    checks.reduce((left, right) => left AND right)
  }
}

object CopyTree extends ((String,List[Column]) => Tree){
  def apply(name: String, params: List[Column]):Tree = if(params.length > 22){
    val args = params.map{ col =>
      PARAM(col.name, col.typeOf.name) := THIS DOT col.name
    }

    DEF("copy") withParams(args) := NEW(name, params.map(x => REF(x.name)): _*)
  }
  else{
    EmptyTree
  }
}
