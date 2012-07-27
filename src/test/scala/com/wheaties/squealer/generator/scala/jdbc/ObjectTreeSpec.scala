package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class ObjectTreeSpec extends Specification{
  "extract" should{

    "handle non-null fields" in{
      val tree = ObjectTree.extract(Column("a", IntType, None, None, ColumnDef) :: Nil, ObjectTree.nakedGet)

      treeToString(tree.head) must be_==("result.getInt(a)")
    }

    "handle null fields" in{
      val tree = ObjectTree.extract(Column("a", IntType, None, None, NullableColumn) :: Nil, ObjectTree.nakedGet)

      treeToString(tree.head) must be_==("Option(result.getInt(a))")
    }

    "handle multiple fields" in{
      val columns = Column("a", IntType, None, None, ColumnDef) :: Column("b", IntType, None, None, ColumnDef) :: Nil
      val tree = ObjectTree.extract(columns, ObjectTree.nakedGet)

      treeToString(tree.head) must be_==("result.getInt(a)")
      treeToString(tree.last) must be_==("result.getInt(b)")
    }
  }

  "namespacedGet" should{
    "handle bynary types" in{
      val tree = ObjectTree.namespacedGet("foo", BinaryType)
      treeToString(tree) must be_==("result.getBytes(ns + foo)")
    }
  }

  "ObjectTree" should{

    "handle non-null fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a))\n  def apply(result: ResultSet, ns: String) = new Foo(result.getInt(ns + a))\n}")
    }

    "handle null fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, NullableColumn) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(Option(result.getInt(a)))\n  def apply(result: ResultSet, ns: String) = new Foo(Option(result.getInt(ns + a)))\n}")
    }

    "handle multiple fields" in{
      val tree = ObjectTree("Foo", Column("a", IntType, None, None, ColumnDef) :: Column("b", IntType, None, None, ColumnDef) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a), result.getInt(b))\n  def apply(result: ResultSet, ns: String) = new Foo(result.getInt(ns + a), result.getInt(ns + b))\n}")
    }
  }
}
