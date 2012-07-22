package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import org.specs2.mutable._
import com.wheaties.squealer.generator.Formato

class ScalaDocsSpec extends Specification{
  val formato = new Formato{
    def databaseName(name: String) = name
    def tableName(name: String) = name
    def columnName(name: String) = name
  }

  def table(columns: List[Column]) = Table("Foo", None, columns)

  "ScalaDocs" should{

    "handle commented columns" in{
      val input = table(Column("foo", IntType, None, Some("bar"), ColumnDef) :: Nil)
      val output = ScalaDocs(input, formato)
      output.exists(_ == "@foo bar") must beTrue
    }
    "handle commented tables" in{
      val input = table(Nil).copy(comment = Some("Something"))
      val output = ScalaDocs(input, formato)
      output.exists(_ == "Something") must beTrue
    }
  }
}










