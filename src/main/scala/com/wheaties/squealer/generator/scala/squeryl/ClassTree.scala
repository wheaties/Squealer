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

package com.wheaties.squealer.generator.scala.squeryl

import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.generator.Formato
import com.wheaties.squealer.db._

object ConstructorTree extends ((String,List[Column],Formato) => ClassDefStart){
  def apply(name: String, columns: List[Column], formato: Formato) ={
    val argList = args(columns, formato)
    val constructor = if(columns.size < 23){
      CASECLASSDEF(formato.tableName(name)) withParams(argList)
    } else {
      CLASSDEF(formato.tableName(name)) withParams(argList)
    }

    if(columns.exists(col => col.colType == PrimaryKey || col.colType == NullablePrimaryKey)){
      val keyed = keyEntity(columns)
      constructor withParents(keyed)
    }
    else{
      constructor
    }
  }

  private[squeryl] def columnAnnot(name: String) = ANNOT("Column", LIT(name))

  private[squeryl] def param(params: List[Column], formato: String => String) ={
    def caseStart(name: String, typeOf: Type):ValDef ={
      PARAM(formato(name), typeOf) withAnnots(columnAnnot(name))
    }
    def classStart(name: String, typeOf: Type):ValDef ={
      VAL(formato(name), typeOf) withAnnots(columnAnnot(name))
    }
    if(params.size < 23) caseStart _ else classStart _
  }

  private[squeryl] def default(params: List[Column], formato: String => String) ={
    def caseDefault(name: String, typeOf: Type, default: Tree):ValDef ={
      PARAM(formato(name), typeOf) withAnnots(columnAnnot(name)) := default
    }
    def classDefault(name: String, typeOf: Type, default: Tree):ValDef ={
      VAL(formato(name), typeOf) withAnnots(columnAnnot(name)) := default
    }

    if(params.size < 23) caseDefault _ else classDefault _
  }

  private[squeryl] def args(params: List[Column], formato: Formato):List[ValDef] ={
    val start = param(params, formato.columnName)
    val withDefault = default(params, formato.columnName)
    params.map{
      _ match{
        case Column(name, typeOf, Some(default), _, ColumnDef | PrimaryKey) => withDefault(name, typeOf.name, defaultArg(typeOf, default))
        case Column(name, typeOf, _, _, ColumnDef | PrimaryKey) => start(name, typeOf.name)
        case Column(name, typeOf, _, _, NullableColumn | NullablePrimaryKey) => start(name, TYPE_OPTION(typeOf.name))
      }
    }
  }

  protected[squeryl] def defaultArg(typeOf: DataType, default: String) ={
    typeOf match{
      case StringType => LIT(default)
      case FloatType if !default.endsWith("f") => REF(default + "f")
      case _ => REF(default)
    }
  }

  private[squeryl] def keyEntity(columns: List[Column])={
    val types:List[Type] = columns flatMap{
      _ match{
        case Column(_, typeOf, _, _, PrimaryKey) => Some(TYPE_REF(typeOf.name))
        case Column(_, typeOf, _, _, NullablePrimaryKey) => Some(TYPE_OPTION(typeOf.name))
        case _ => None
      }
    }

    val keyed = RootClass.newClass("KeyedEntity")
    types match{
      case List(item) => keyed TYPE_OF(item)
      case _ =>
        val composite = RootClass.newTypeParameter("CompositeKey%s".format(types.size)) TYPE_OF(types)
        keyed TYPE_OF(composite)
    }
  }
}

object DefinitionsTree extends (List[Column] => List[Tree]){

  def apply(columns: List[Column])={
    if(columns.exists(_.colType == NullablePrimaryKey)){
      id(columns) :: optionConstructor(columns) :: Nil
    }
    else if(columns.exists(_.colType == PrimaryKey)){
      id(columns) :: Nil
    }
    else if(columns.exists(_.colType == NullableColumn)){
      optionConstructor(columns) :: Nil
    }
    else{
      Nil
    }
  }

  protected[squeryl] def optionConstructor(columns: List[Column]):Tree ={
    val args = columns map{
      _ match{
        case Column(_, typeOf, Some(default), _, NullableColumn | NullablePrimaryKey) => SOME(defaultArg(typeOf, default))
        case Column(_, typeOf, Some(default), _, _) => defaultArg(typeOf, default)
        case Column(_, typeOf, _, _, NullableColumn | NullablePrimaryKey) => SOME(defaultArg(typeOf))
        case Column(_, typeOf, _, _, _) => defaultArg(typeOf)
      }
    }
    DEFTHIS := THIS APPLY(args)
  }

  protected[squeryl] def defaultArg(typeOf: DataType, default: String) ={
    typeOf match{
      case StringType => LIT(default)
      case FloatType if !default.endsWith("f") => REF(default + "f")
      case _ => REF(default)
    }
  }

  //TODO: check mapping against Squeryl
  protected[squeryl] def defaultArg(typeOf: DataType) = typeOf match{
    case ArrayType => REF("null") //I have no other way :(
    case BinaryType => NEW( TYPE_ARRAY("Byte") )
    case BlobType => NEW("SerialBlob", NEW( TYPE_ARRAY("Byte") ))
    case BooleanType => FALSE
    case ClobType => NEW("SerialClob", NEW( TYPE_ARRAY("Char") ))
    case DateType => NEW("Date", LIT(0))
    case DecimalType | DoubleType => LIT(0.0)
    case FloatType => LIT(0.0f)
    case IntType | ShortType => LIT(0)
    case LongType => LIT(0L)
    case ObjectType | UnknownType => NEW("Any")
    case StringType => LIT("")
    case TimestampType => NEW("Timestamp", LIT(0))
    case TimeType => NEW("Time", LIT(0))
  }

  protected[squeryl] def id(columns: List[Column]):Tree ={
    val keys = for{
      column <- columns if column.colType == PrimaryKey || column.colType == NullablePrimaryKey
    } yield REF(column.name)

    DEF("id") := REF("compositeKey") APPLY(keys: _*)
  }
}