package com.wheaties.squealer.generator.scala.squeryl

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._

//TODO: normal plus keyed entity
class ConstructorTree{
  def apply(name: String, columns: List[Column], formato: String => String) ={
    val argList = args(columns, formato)
    val constructor = if(columns.size < 23){
      CASECLASSDEF(name) withParams(argList)
    } else{
      CLASSDEF(name) withParams(argList)
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
      PARAM(formato(name), typeOf) := LIT(default) withAnnots(columnAnnot(name))
    }
    def classDefault(name: String, typeOf: Type, default: String):ValDef ={
      VAL(formato(name), typeOf) := LIT(default) withAnnots(columnAnnot(name))
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
    if(types.size > 1){
      keyed TYPE_OF(types)
    }
    else{
      val composite = RootClass.newTypeParameter("CompositeKey%s".format(types.size)) TYPE_OF(types)
      keyed TYPE_OF(composite)
    }
  }
}