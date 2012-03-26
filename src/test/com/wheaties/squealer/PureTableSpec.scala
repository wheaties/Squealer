package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class PureTableSpec extends Specification{
  "PureTable" should{

    "handle a tree with a primary key" in{
      val tree = PureTable(TableTree("Foo", PrimaryKeyDef("a", "Int", None) :: Nil))

      treeToString(tree) must be_==("case class Foo(a: Int) {\n  override def hashCode = List(a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)\n  override def equals(that: Any) =\n    that match {\n      case Foo(a) => this.a eq that.a\n      case _ => false\n    }\n}")
    }

    "handle a tree without a primary key" in{
      val columns = for{indx <- 0 to 23} yield ColumnDef("a", "Int", None)
      val tree = PureTable(TableTree("Foo", columns.toList))

      treeToString(tree) must be_==("class Foo(a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int, a: Int) {}\n}")
    }
  }
}
