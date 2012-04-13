package com.wheaties.squealer

import treehugger.forest._
import treehuggerDSL._

object PureTable extends (Table => Tree){
  def apply(table: Table) ={
    BLOCK{
      ObjectTree(table.name, table.columns)
      ToTree(table.name, table.columns)
    } withoutPackage
  }
}

//TODO: move me to where we do the SQL AST parsing and matching against database
case class Clazz(classPackage: String, name: String, columns: List[Column])

object ImpureTable extends (Clazz => Tree){
  def apply(clazz: Clazz) ={
    BLOCK{
      ObjectTree(clazz.name, clazz.columns)
      ToTree(clazz.name, clazz.columns)
    } inPackage(clazz.classPackage)
  }
}

//TODO: why don't regular classes have hashCode and equals!?
object ToTree extends ((String, List[Column]) => ClassDef){
  def apply(name: String, columns: List[Column]):ClassDef ={
    val copy = if(columns.size < 23) Nil else CopyTree(name, columns) :: Nil
    val body = if(hasPrimaryKey(columns)){
      HashCodeTree(columns) :: EqualsTree(name, columns) :: Nil
    }
    else Nil

    ConstructorTree(name, columns) := BLOCK{ AssumptionTree(columns) ::: body ::: copy }
  }

  protected def hasPrimaryKey(columns: List[Column]) ={
    columns.exists(_.isInstanceOf[PrimaryKeyDef]) || columns.exists(_.isInstanceOf[NullablePrimaryKey])
  }
}