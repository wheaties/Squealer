package com.wheaties.squealer.generator.scala.squeryl

import org.specs2.mutable.Specification
import com.wheaties.squealer.generator.Formato
import com.wheaties.squealer.db._
import treehugger.forest._

class SquerylGeneratorSpec extends Specification{
  val formato = new Formato{
    def databaseName(name: String) = name
    def tableName(name: String) = name
    def columnName(name: String) = name
  }

  "writeTable" should{
    val table = Table("foo", None, Column("bar", IntType, None, None, PrimaryKey) :: Nil)
    val output = SquerylGenerator.writeTable(table, "baz", formato)

    "produce import statements" in{
      output must contain("org.squeryl.PrimitiveTypeMode._") and contain("org.squeryl.annotations.Column")
    }

    "produce the table object" in{
      output must contain("case class foo")
    }

    "produce the package the table object is contained" in{
      output must contain("package baz")
    }
  }

  "makeClass" should{
    val table = Table("foo", None, Column("bar", IntType, None, None, NullablePrimaryKey) :: Nil)
    val output = SquerylGenerator.makeClass(table.name, table.columns, Nil, formato)

    "produce a class" in{
      treeToString(output) must contain("class foo")
    }

    "produce the member functions of a class" in{
      treeToString(output) must contain("def id = compositeKey(bar)") and contain("def this = this(Some(0))")
    }
  }

  "writeDatabase" should{
    val table = Table("foo", None, Column("bar", IntType, None, None, PrimaryKey) :: Nil)
    val db = Database("baz", table :: Nil)
    val output = SquerylGenerator.writeDatabase(db, "baz", formato)

    "produce import statements" in{
      output must contain("org.squeryl.PrimitiveTypeMode._") and contain("org.squery.Schema")
    }

    "handle a schema with one table" in{
      output must contain("object baz extends Schema {\n  val foo = table[foo](\"foo\")\n}")
    }

    "handle a schema with multiple tables" in{
      val multi = Database("baz", table :: table.copy(name = "yo") :: Nil)
      val output = SquerylGenerator.writeDatabase(multi, "baz", formato)

      output must contain("object baz extends Schema {\n  val foo = table[foo](\"foo\")\n  val yo = table[yo](\"yo\")\n}")
    }
  }

  "tableDef" should{
    "produce something" in{
      val output = SquerylGenerator.tableDef("foo", formato)

      treeToString(output) must be_==("val foo = table[foo](\"foo\")")
    }
  }
}