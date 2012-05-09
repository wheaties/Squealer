package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class PureTableSpec extends Specification{
  "PureTable with a primary key" should{
    val tree = PureTable(Table("Foo", None, Column("a", "Int", None, None, PrimaryKey) :: Nil), "")

    "produce a case class" in{
      treeToString(tree) must contain("case class Foo(a: Int) {")
    }

    "produce a hashCode" in{
      treeToString(tree) must contain("override def hashCode ")
    }

    "produce an equals" in{
      treeToString(tree) must contain("override def equals(that: Any)")
    }

    "produce an object" in{
      treeToString(tree) must contain("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a))\n}")
    }

    "import java.sql" in{
      treeToString(tree) must contain("import java.sql._")
    }
  }

  "Pure table with more than 22 fields and no primary key" should{
    val columns = for{indx <- 1 to 23} yield Column("a" + indx.toString, "Int", None, None, ColumnDef)
    val tree = PureTable(Table("Foo", None, columns.toList), "")

    "produce a class" in{
      treeToString(tree) must contain("class Foo(val a1: Int, val a2: Int, val a3: Int, val a4: Int, val a5: Int, val a6: Int, val a7: Int, val a8: Int, val a9: Int, val a10: Int, val a11: Int, val a12: Int, val a13: Int, val a14: Int, val a15: Int, val a16: Int, val a17: Int, val a18: Int, val a19: Int, val a20: Int, val a21: Int, val a22: Int, val a23: Int) {")
    }

    "produce a copy" in{
      treeToString(tree) must contain("def copy")
    }

    "produce a hashCode" in{
      treeToString(tree) must contain("override lazy val hashCode =")
    }

    "produce an equals" in{
      treeToString(tree) must contain("def equals(that: Any) =")
    }

    "produce an object" in{
      treeToString(tree) must contain("object Foo {\n  def apply(result: ResultSet) = new Foo(result.getInt(a1), result.getInt(a2), result.getInt(a3), result.getInt(a4), result.getInt(a5), result.getInt(a6), result.getInt(a7), result.getInt(a8), result.getInt(a9), result.getInt(a10), result.getInt(a11), result.getInt(a12), result.getInt(a13), result.getInt(a14), result.getInt(a15), result.getInt(a16), result.getInt(a17), result.getInt(a18), result.getInt(a19), result.getInt(a20), result.getInt(a21), result.getInt(a22), result.getInt(a23))\n}")
    }

    "import java.sql" in{
      treeToString(tree) must contain("import java.sql._")
    }
  }
}
