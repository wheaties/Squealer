package com.wheaties.squealer

import treehugger.forest._
import treehuggerDSL._

object PureTable extends ((Table, String, Table => Table) => Tree){
  def apply(table: Table, pack: String, formatter : (Table => Table) = identity):Tree = if(pack.isEmpty){
    BLOCK{ block(formatter(table)) } withoutPackage
  }
  else{
    BLOCK{ block(formatter(table)) } inPackage(pack)
  }

  protected[squealer] def block(table: Table)={
    IMPORT("java.sql._") ::
    ObjectTree(table.name, table.columns) ::
    ToTree(table.name, table.columns) :: Nil
  }
}

//TODO: move me to where we do the SQL AST parsing and matching against database
case class Clazz(classPackage: String, name: String, sql: String, columns: List[Column])

//TODO: have to include the statement creators here to map the SQL to code
object ImpureTable extends (Clazz => Tree){
  def apply(clazz: Clazz) = BLOCK{
    IMPORT("java.sql._") ::
    ObjectTree(clazz.name, clazz.columns) ::
    ToTree(clazz.name, clazz.columns) :: Nil
  } inPackage(clazz.classPackage)
}

object ToTree extends ((String, List[Column]) => Tree){
  def apply(name: String, columns: List[Column]):Tree ={
    val comments = ScalaDocTree(name, columns)
    comments(makeClass(name, columns))
  }

  private[squealer] def makeClass(name: String, columns: List[Column]):ClassDef ={
    ConstructorTree(name, columns) := BLOCK{
      AssumptionTree(columns) ::: HashCodeTree(columns) :: EqualsTree(name, columns) ::  CopyTree(name, columns) :: Nil
    }
  }
}