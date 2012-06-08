package com.wheaties.squealer.db

trait Lens[A,B] extends Function2[A,B,A]{
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

object ColumnTypeLens extends ColumnLens[ColumnType]{
  protected def get(column: Column) = column.colType
  protected def set(column: Column, value: ColumnType) = column.copy(colType = value)
}