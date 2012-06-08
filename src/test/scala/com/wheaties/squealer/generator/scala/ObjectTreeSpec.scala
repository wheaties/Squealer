package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class ObjectTreeSpec extends Specification{
  "ObjectTree extract" should{

    "handle non-null fields" in{
      val tree = ObjectTree.extract(Column("a", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree.head) must be_==("result.getInt(a)")
    }

    "handle null fields" in{
      val tree = ObjectTree.extract(Column("a", IntType, None, None, NullableColumn) :: Nil)

      treeToString(tree.head) must be_==("Option(result.getInt(a))")
    }

    "handle multiple fields" in{
      val tree = ObjectTree.extract(Column("a", IntType, None, None, ColumnDef) :: Column("b", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree.head) must be_==("result.getInt(a)")
      treeToString(tree.last) must be_==("result.getInt(b)")
    }
  }

  "ObjectTree" should{

    "handle non-null fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a))\n}")
    }

    "handle null fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, NullableColumn) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(Option(result.getInt(a)))\n}")
    }

    "handle multiple fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, ColumnDef) :: Column("b", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a), result.getInt(b))\n}")
    }
  }
}
