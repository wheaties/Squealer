package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import scala.annotation._

object PureTable extends (TableTree => Tree){

  def apply(table: TableTree) ={
    val copy = if(table.columnCount < 23) Nil else CopyTree(table.name, table.columns) :: Nil
    val body = if(table.hasPrimaryKey){
      HashCodeTree(table.columns) :: EqualsTree(table.name, table.columns) :: Nil
    }
    else Nil

    ConstructorTree(table.name, table.columns) := BLOCK{ AssumptionTree(table.columns) ::: body ::: copy }
  }
}
