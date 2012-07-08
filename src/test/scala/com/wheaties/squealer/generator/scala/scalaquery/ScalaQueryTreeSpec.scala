package com.wheaties.squealer.generator.scala.scalaquery

import org.specs2.mutable.Specification
import treehugger.forest._
import com.wheaties.squealer.db.{NullableColumn, ColumnDef, IntType, Column}

class ScalaQueryTreeSpec extends Specification{
  val column = Column("bar", IntType, None, None, ColumnDef)

  "declaration" should{

    "handle one column" in{
      val tree = ScalaQueryTree.declaration("Foo", column :: Nil)
      treeToString(tree.tree) must be_==("object Foo extends BasicTable[Int](\"Foo\")")
    }

    "handle multiple columns" in{
      val tree = ScalaQueryTree.declaration("Foo", column :: column :: Nil)
      treeToString(tree.tree) must be_==("object Foo extends BasicTable[(Int, Int)](\"Foo\")")
    }
  }

  "projection" should{

    "handle one column" in{
      val tree = ScalaQueryTree.projection(column :: Nil)
      treeToString(tree) must be_==("def * = bar")
    }

    "handle multiple columns" in{
      val tree = ScalaQueryTree.projection(column :: column :: column :: Nil)
      treeToString(tree) must be_==("def * = bar.~(bar.~(bar))")
    }
  }

  "member" should{

    "handle non-null columns" in{
      val tree = ScalaQueryTree.member(column)
      treeToString(tree) must be_==("def bar = column[Int](\"bar\", 0 NotNull)")
    }
    "handle null columns" in{
      val tree = ScalaQueryTree.member(column.copy(colType = NullableColumn))
      treeToString(tree) must be_==("def bar = column[Int](\"bar\")")
    }
    "handle default columns" in{
      val tree = ScalaQueryTree.member(column.copy(default = Some("1"), colType = NullableColumn))
      treeToString(tree) must be_==("def bar = column[Int](\"bar\", 0 Default Some(1))")
    }
    "handle non-null, default columns" in{
      val tree = ScalaQueryTree.member(column.copy(default = Some("1")))
      treeToString(tree) must be_==("def bar = column[Int](\"bar\", 0 NotNull, 0 Default 1)")
    }
  }

  "apply" should{
    "produce a table with one column" in{
      val tree = ScalaQueryTree("Foo", column :: Nil)
      treeToString(tree) must be_==("object Foo extends BasicTable[Int](\"Foo\") {\n  def * = bar\n  def bar = column[Int](\"bar\", 0 NotNull)\n}")
    }
    "produce a table with multiple columns" in{
      val tree = ScalaQueryTree("Foo", column :: column :: column :: Nil)
      treeToString(tree) must be_==("object Foo extends BasicTable[(Int, Int, Int)](\"Foo\") {\n  def * = bar.~(bar.~(bar))\n  def bar = column[Int](\"bar\", 0 NotNull)\n  def bar = column[Int](\"bar\", 0 NotNull)\n  def bar = column[Int](\"bar\", 0 NotNull)\n}")
    }
  }
}