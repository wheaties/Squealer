package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import org.specs2.mutable._

class ScalaDocsSpec extends Specification{
  def table(columns: List[Column]) = Table("Foo", None, columns)

  "ScalaDocs" should{

    "handle commented columns" in{
      val input = table(Column("foo", IntType, None, Some("bar"), ColumnDef) :: Nil)
      val output = ScalaDocs(input, identity)
      output.exists(_ == "@foo bar") must beTrue
    }
    "handle commented tables" in{
      val input = table(Nil).copy(comment = Some("Something"))
      val output = ScalaDocs(input, identity)
      output.exists(_ == "Something") must beTrue
    }
  }
}










