package com.wheaties.squealer.generator.scala.squeryl

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._

object ConstructorTree extends ((String,List[Column],String => String) => ClassDefStart){
  def apply(name: String, columns: List[Column], formato: String => String) ={
    val argList = args(columns, formato)
    val constructor = if(columns.size < 23){
      CASECLASSDEF(name.capitalize) withParams(argList)
    } else {
      CLASSDEF(name.capitalize) withParams(argList)
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
    def caseDefault(name: String, typeOf: Type, default: String):ValDef ={
      PARAM(formato(name), typeOf) withAnnots(columnAnnot(name)) := REF(default)
    }
    def classDefault(name: String, typeOf: Type, default: String):ValDef ={
      VAL(formato(name), typeOf) withAnnots(columnAnnot(name)) := REF(default)
    }

    if(params.size < 23) caseDefault _ else classDefault _
  }

  private[squeryl] def args(params: List[Column], formato: String => String):List[ValDef] ={
    val start = param(params, formato)
    val withDefault = default(params, formato)
    params.map{
      _ match{
        case Column(name, typeOf, Some(default), _, ColumnDef | PrimaryKey) => withDefault(name, typeOf.name, default)
        case Column(name, typeOf, _, _, ColumnDef | PrimaryKey) => start(name, typeOf.name)
        case Column(name, typeOf, _, _, NullableColumn | NullablePrimaryKey) => start(name, TYPE_OPTION(typeOf.name))
      }
    }
  }

  private[squeryl] def keyEntity(columns: List[Column])={
    val types:List[Type] = columns.flatMap{
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

object DefinitionsTree extends ((List[Column],String => String) => List[Tree]){

  def apply(columns: List[Column], formato: String => String)={
    if(columns.exists(col => col.colType == PrimaryKey || col.colType == NullablePrimaryKey)){
      id(columns, formato) :: optionConstructor (columns) :: Nil
    }
    else{
      optionConstructor (columns) :: Nil
    }
  }

  protected[squeryl] def optionConstructor(columns: List[Column]):Tree ={
    val args = columns map{
      _ match{
        case Column(_, _, Some(default), _, NullableColumn | NullablePrimaryKey) => SOME(REF(default))
        case Column(_, _, Some(default), _, _) => REF(default)
        case Column(_, typeOf, _, _, NullableColumn | NullablePrimaryKey) => SOME(defaultArg(typeOf))
        case Column(_, typeOf, _, _, _) => defaultArg(typeOf)
      }
    }
    DEF("this") := THIS APPLY(args)
  }

  //TODO: check mapping against Squeryl
  protected[squeryl] def defaultArg(typeOf: DataType) = typeOf match{
    case ArrayType => REF("null") //I have no other way :(
    case BinaryType => NEW( TYPE_ARRAY("Byte") )
    case BlobType => NEW("SerialBlob", NEW( TYPE_ARRAY("Byte") ))
    case BooleanType => FALSE
    case ClobType => NEW("SerialClob", NEW( TYPE_ARRAY("Char") ))
    case DateType => NEW("Date", LIT(0))
    case DecimalType | DoubleType | FloatType => LIT(0.0)
    case IntType | ShortType => LIT(0)
    case LongType => LIT(0L)
    case ObjectType | UnknownType => NEW("Any")
    case StringType => LIT("")
    case TimestampType => NEW("Timestamp", LIT(0))
    case TimeType => NEW("Time", LIT(0))
  }

  protected[squeryl] def id(columns: List[Column], formato: String => String):Tree ={
    val keys = for{
      column <- columns if column.colType == PrimaryKey || column.colType == NullablePrimaryKey
    } yield REF(column.name)

    DEF("id") := REF("compositeKey") APPLY(keys: _*)
  }
}