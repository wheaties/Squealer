package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class PureTableSpec extends Specification{
  "PureTable" should{

    "handle a tree with a primary key" in{
      val tree = PureTable(Table("Foo", PrimaryKeyDef("a", "Int", None) :: Nil))

      treeToString(tree) must be_==("case class Foo(a: Int) {\n  override def hashCode = List(a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)\n  override def equals(that: Any) =\n    that match {\n      case Foo(a) => this.a eq that.a\n      case _ => false\n    }\n}")
    }

    "handle a tree with more than 22 fields" in{
      val columns = for{indx <- 1 to 23} yield ColumnDef("a" + indx.toString, "Int", None)
      val tree = PureTable(Table("Foo", columns.toList))

      treeToString(tree) must be_==("class Foo(val a1: Int, val a2: Int, val a3: Int, val a4: Int, val a5: Int, val a6: Int, val a7: Int, val a8: Int, val a9: Int, val a10: Int, val a11: Int, val a12: Int, val a13: Int, val a14: Int, val a15: Int, val a16: Int, val a17: Int, val a18: Int, val a19: Int, val a20: Int, val a21: Int, val a22: Int, val a23: Int) {\n  def copy(a1: Int = this.a1, a2: Int = this.a2, a3: Int = this.a3, a4: Int = this.a4, a5: Int = this.a5, a6: Int = this.a6, a7: Int = this.a7, a8: Int = this.a8, a9: Int = this.a9, a10: Int = this.a10, a11: Int = this.a11, a12: Int = this.a12, a13: Int = this.a13, a14: Int = this.a14, a15: Int = this.a15, a16: Int = this.a16, a17: Int = this.a17, a18: Int = this.a18, a19: Int = this.a19, a20: Int = this.a20, a21: Int = this.a21, a22: Int = this.a22, a23: Int = this.a23) = new Foo(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)\n}")
    }
  }
}
