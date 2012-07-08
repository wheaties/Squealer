package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import com.wheaties.squealer.generator.scala.jdbc._
import treehugger.forest._
import treehuggerDSL._

//TODO: move this to jdbc package
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