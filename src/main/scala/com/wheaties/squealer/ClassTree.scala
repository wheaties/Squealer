package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import scala.annotation._

case class DatabaseTree(name: String, tables: List[TableTree])
case class TableTree(name: String, columns: List[Column]){
  val hasPrimaryKey ={
    columns.exists(_.isInstanceOf[PrimaryKeyDef]) || columns.exists(_.isInstanceOf[NullablePrimaryKey])
  }
  def columnCount = columns.size
}

sealed trait Column{
  val name: String
  val typeOf: String
  def size: Int = 0
  def precision: Int = 0
  def scale: Int = 0
  def length: Int = 0
}
trait WithSize{
  val size: Int
}
trait WithScale{
  val scale: Int
  val precision: Int
}
trait WithLength{
  val length: Int
}

case class ColumnDef(name: String, typeOf: String, default: Option[String]) extends Column
case class NullableColumnDef(name: String, typeOf: String) extends Column
case class PrimaryKeyDef(name: String, typeOf: String, default: Option[String]) extends Column
case class NullablePrimaryKey(name: String, typeOf: String) extends Column

//TODO: forgot the object apply which does a ResultSet creation of the case class/class
object ConstructorTree {
  def apply(name: String, params: List[Column]):ClassDef ={
    val start = if(params.size < 23) CASECLASSDEF(name) else CLASSDEF(name)
    start withParams(make(params, Nil))
  }

  @tailrec private[squealer] def make(remainder: List[Column], acc: List[ValDef]):List[ValDef] = remainder match{
    case ColumnDef(name, typeOf, Some(default)) :: xs =>
      val param = PARAM(name, typeOf) := LIT(default)
      make(xs, param :: acc)
    case ColumnDef(name, typeOf, _) :: xs =>
      val param = PARAM(name, typeOf)
      make(xs, param :: acc)
    case NullableColumnDef(name, typeOf) :: xs =>
      val param = PARAM(name, TYPE_OPTION(typeOf))
      make(xs, param :: acc)
    case NullablePrimaryKey(name, typeOf) :: xs =>
      val param = PARAM(name, TYPE_OPTION(typeOf))
      make(xs, param :: acc)
    case PrimaryKeyDef(name, typeOf, Some(default)) :: xs =>
      val param = PARAM(name, typeOf) := LIT(default)
      make(xs, param :: acc)
    case PrimaryKeyDef(name, typeOf, _) :: xs =>
      val param = PARAM(name, typeOf)
      make(xs, param :: acc)
    case Nil => acc.reverse
  }
}

object AssumptionTree extends (List[Column] => List[Tree]){
  def apply(params: List[Column]) = {
    @tailrec def make(remainder: List[Column], acc: List[Tree] = Nil):List[Tree] = remainder match{
      case NullableColumnDef(name, _) :: xs if check(remainder.head) =>
        val assumeOpt = REF(name) MAP LAMBDA(PARAM("x")) ==> makeAssumption(remainder.head, "x")
        make(xs, assumeOpt :: acc)
      case NullablePrimaryKey(name, _) :: xs if check(remainder.head) =>
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
  def apply(params: List[Column])= DEF("hashCode") withFlags(Flags.OVERRIDE) := make(params)

  @tailrec private[squealer] def make(remainder: List[Column], acc: List[SelectStart] = Nil):Tree = remainder match{
    case PrimaryKeyDef(name, typeOf, _) :: xs =>
      val key = REF(name) DOT "hashCode"
      make(xs, key :: acc)
    case NullablePrimaryKey(name, _) :: xs =>
      val key = REF(name) DOT "hashCode"
      make(xs, key :: acc)
    case x :: xs => make(xs, acc)
    case Nil =>
      if(acc.isEmpty) throw new UnsupportedOperationException
      else LIST(acc) REDUCELEFT LAMBDA(PARAM(TUPLE(REF("left"), REF("right")))) ==>{
        PAREN(REF("left") INT_* LIT(17)) INFIX("^") APPLY REF("right")
      }
  }
}

object EqualsTree extends ((String, List[Column]) => Tree){
  def apply(tableName: String, params: List[Column]) ={
    DEF("equals") withFlags(Flags.OVERRIDE) withParams(PARAM("that", "Any")) := REF("that") MATCH{
      (CASE(REF(tableName) UNAPPLY pattern(params, Nil)) ==>(make(params, Nil))) ::
      (CASE(WILDCARD) ==> FALSE) :: Nil
    }
  }

  @tailrec private[squealer] def make(remainder: List[Column], acc: List[Infix]):Infix = remainder match{
    case PrimaryKeyDef(name, _, _) :: xs =>
      val param = (THIS DOT name) OBJ_EQ (REF("that") DOT name)
      make(xs, param :: acc)
    case x :: xs => make(xs, acc)
    case Nil => acc.reduce((left, right) => left AND right)
  }

  @tailrec private[squealer] def pattern(remainder: List[Column], acc: List[Ident]):List[Ident] = remainder match{
    case PrimaryKeyDef(name, _, _) :: xs => pattern(xs, ID(name) :: acc)
    case NullablePrimaryKey(name, _) :: xs => pattern(xs, ID(name) :: acc)
    case x :: xs => pattern(xs, WILDCARD :: acc)
    case Nil => acc.reverse
  }
}

object CopyTree extends ((String,List[Column]) => Tree){
  def apply(name: String, params: List[Column]):Tree ={
    val args = params.map{ col =>
      PARAM(col.name, col.typeOf) := THIS DOT col.name
    }

    DEF("copy") withParams(args) := NEW(name, params.map(x => REF(x.name)): _*)
  }
}
