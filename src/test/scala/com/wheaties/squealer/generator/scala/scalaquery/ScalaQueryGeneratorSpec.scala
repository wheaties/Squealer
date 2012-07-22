package com.wheaties.squealer.generator.scala.scalaquery

import org.specs2.mutable.Specification
import com.wheaties.squealer.generator.Formato
import com.wheaties.squealer.db.{ColumnDef, IntType, Column, Table}

class ScalaQueryGeneratorSpec extends Specification{
  val formato = new Formato{
    def databaseName(name: String) = name
    def tableName(name: String) = name
    def columnName(name: String) = name
  }

  "writeTable" should{
    val table = Table("foo", None, Column("bar", IntType, None, None, ColumnDef) :: Nil)
    val output = ScalaQueryGenerator.writeTable(table, "foo", formato)

    "produce import statements" in{
      output must contain("import org.scalaquery.ql.basic.BasicTable") and contain("org.scalaquery.ql.TypeMapper._")
    }

    "produce the table object" in{
      output must contain("object foo extends BasicTable[Int]")
    }

    "produce the package the table object is contained" in{
      output must contain("package foo")
    }
  }
}