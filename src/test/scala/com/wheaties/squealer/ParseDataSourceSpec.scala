package com.wheaties.squealer

import org.specs2.mutable.Specification
import java.sql.{DatabaseMetaData, DriverManager}

class ParseDataSourceSpec extends Specification{

  Class.forName("org.hsqldb.jdbcDriver")
  def connection = DriverManager.getConnection("jdbc:hsqldb:mem:aname", "SA", "")

  "parseDatabase" should{

    "handle DB with meta tables" in{
      val output = ParseDataSource.parseDatabase("foo", connection)
      output.tables must not beEmpty
    }
  }

  "parseTables" should{
    val con = connection
    val statement = con.createStatement()
    statement.execute("CREATE TABLE FOO(id INTEGER)")
    statement.executeUpdate("INSERT INTO FOO(id) VALUES(1)")

    "produce a known table" in{
      val output = ParseDataSource.parseTables(con.getMetaData)
      output.find(_.name == "FOO") must not beNone
    }
  }

  "parsePrimaryKeys" should{
    val con = connection
    val statement = con.createStatement()
    statement.execute("CREATE TABLE BAR(id INTEGER PRIMARY KEY NOT NULL)")

    "produce a known primary key" in{
      val output = ParseDataSource.parsePrimaryKeys(con.getMetaData, "BAR")
      output.find(_ == "ID") must not beNone
    }
  }

  "parseColumns" should{
    val con = connection

    "produce a primary key column" in{
      val output = ParseDataSource.parseColumns(con.getMetaData, "BAR")
      val head = output.headOption
      head.map(_.name).getOrElse("") must be_==("ID")
      head.exists(_.colType == PrimaryKey) must beTrue
    }

    "produce a nullable column" in{
      val output = ParseDataSource.parseColumns(con.getMetaData, "FOO")
      val head = output.headOption
      head.map(_.name).getOrElse("") must be_==("ID")
      head.exists(_.colType == NullableColumn) must beTrue
    }
  }
}