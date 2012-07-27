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

package com.wheaties.squealer.db

//TODO: Perhaps I need to move Lens and Result to a untils package
trait Lens[A,B] extends ((A,B) => A){
  self =>

  protected def get(a: A):B
  protected def set(a: A, value: B):A

  def modify(a:A, func: B => B):A = apply(a, func(get(a)))
  def apply(a: A, value: B) = set(a, value)

  def compose[C](that: Lens[C,A]) = new Lens[C,B]{
    def get(c: C) = self.get(that.get(c))
    def set(c: C, b: B) = that.modify(c, self(_, b))
  }
  def andThen[C](that: Lens[B,C]) = that compose this
}

trait ColumnLens[B] extends Lens[Column,B]{
  override def apply(column: Column, value: B) ={
    val Column(name, typeOf, default, comment, colType) = set(column, value)

    new Column(name, typeOf, default, comment, colType){
      override val size: Int = column.size
      override val precision: Int = column.precision
      override val scale: Int = column.scale
      override val length: Int = column.length
    }
  }
}

object ColumnNameLens extends ColumnLens[String]{
  protected def get(column: Column) = column.name
  protected def set(column: Column, value: String) = column.copy(name = value)
}

//TODO: This even needed anymore? Would have been good for sql parsing but no use now, no?
object ColumnTypeLens extends ColumnLens[ColumnType]{
  protected def get(column: Column) = column.colType
  protected def set(column: Column, value: ColumnType) = column.copy(colType = value)
}

object TableNameLens extends Lens[Table,String]{
  protected def get(table: Table) = table.name
  protected def set(table: Table, value: String) = table.copy(name = value)
}

object DatabaseTableLens extends Lens[Database,List[Table]]{
  protected def get(db: Database) = db.tables
  protected def set(db: Database, value: List[Table]) = db.copy(tables = value)
}