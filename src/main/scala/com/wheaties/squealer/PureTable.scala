package com.wheaties.squealer

import treehugger.forest._
import treehuggerDSL._

//TODO: D-R-Y violations here
object PureTable extends (Table => Tree){
  def apply(table: Table) ={
    BLOCK{
      IMPORT("java.sql._") ::
      ObjectTree(table.name, table.columns) ::
      ToTree(table.name, table.columns) :: Nil
    } withoutPackage
  }
}

//TODO: move me to where we do the SQL AST parsing and matching against database
case class Clazz(classPackage: String, name: String, columns: List[Column])

object ImpureTable extends (Clazz => Tree){
  def apply(clazz: Clazz) ={
    BLOCK{
      IMPORT("java.sql._") ::
      ObjectTree(clazz.name, clazz.columns) ::
      ToTree(clazz.name, clazz.columns) :: Nil
    } inPackage(clazz.classPackage)
  }
}

//TODO: this isn't an object. it has one method, apply. It's a def. Make it so...
object ToTree extends ((String, List[Column]) => ClassDef){
  def apply(name: String, columns: List[Column]):ClassDef ={
    ConstructorTree(name, columns) := BLOCK{
      AssumptionTree(columns) ::: HashCodeTree(columns) :: EqualsTree(name, columns) ::  CopyTree(name, columns) :: Nil
    }
  }
}