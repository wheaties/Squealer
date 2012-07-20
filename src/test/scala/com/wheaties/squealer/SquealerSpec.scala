package com.wheaties.squealer

import config.TableParams
import db._
import org.specs2.mutable.Specification
import treehugger.forest._
import definitions._
import treehuggerDSL._
import java.sql.DriverManager

/**
 * Part unit test, part integration test. Mostly integration test.
 */
class SquealerSpec extends Specification{
  val squeal = new Object with Squealer

  "generateTable" should{
    val db = Database("foo", Table("foo", None, Column("bar", IntType, None, None, ColumnDef) :: Nil) :: Nil)

    "produce a named table" in{
      val statement = TableParams("com.wheaties", "foo")
      true must beFalse
    }
  }

  Class.forName("org.hsqldb.jdbcDriver")

  def connection = DriverManager.getConnection("jdbc:hsqldb:mem:aname", "SA", "")

  class SideEffectTester extends Recorder[ParsedResult]{
    val builder = new StringBuilder

    def record(result: ParsedResult){
      builder ++= treeToString(result.ast)
    }
  }

  "action" should{
    val con = connection
    val statement = con.createStatement()
    statement.execute("CREATE TABLE FOOBAR(id INTEGER, value INTEGER)")

    /*"generate a table when instructed" in{
      val side = new SideEffectTester
      squeal.action("test.conf")(side)
      val out = side.builder.toString

      out must be_==(
        """import java.sql._
        object Foobar{
          def apply(result: ResultSet) = new Foobar(result.geInt("id"), result.getInt("value"))
        }
        case class Foobar(id: Int, value: Int)"""
      )
    }*/
  }
}