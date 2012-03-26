package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class ObjectTreeSpec extends Specification{
  "ObjectTree extract" should{

    "handle non-null fields" in{
      val tree = ObjectTree.extract(ColumnDef("a", "Int", None) :: Nil)

      treeToString(tree.head) must be_==("result.getInt(a)")
    }

    "handle null fields" in{
      val tree = ObjectTree.extract(NullableColumnDef("a", "Int") :: Nil)

      treeToString(tree.head) must be_==("Option(result.getInt(a))")
    }

    "handle multiple fields" in{
      val tree = ObjectTree.extract(ColumnDef("a", "Int", None) :: ColumnDef("b", "Int", None) :: Nil)

      treeToString(tree.head) must be_==("result.getInt(a)")
      treeToString(tree.last) must be_==("result.getInt(b)")
    }
  }

  "ObjectTree" should{

    "handle non-null fields" in{
      val tree = ObjectTree("Foo", ColumnDef("a", "Int", None) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a))\n}")
    }

    "handle null fields" in{
      val tree = ObjectTree("Foo", NullableColumnDef("a", "Int") :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(Option(result.getInt(a)))\n}")
    }

    "handle multiple fields" in{
      val tree = ObjectTree("Foo", ColumnDef("a", "Int", None) :: ColumnDef("b", "Int", None) :: Nil)

      treeToString(tree) must be_==("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a), result.getInt(b))\n}")
    }
  }
}
