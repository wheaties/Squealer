package com.wheaties.squealer

import config.TableParams
import db._
import org.specs2.mutable.Specification
import treehugger.forest._
import definitions._
import treehuggerDSL._
import java.sql.DriverManager

//TODO: Look into the Specs2 way of checking on file creation
class SquealerSpec extends Specification{
  val squeal = new Object with Squealer

  Class.forName("org.hsqldb.jdbcDriver")

  def connection = DriverManager.getConnection("jdbc:hsqldb:mem:aname", "SA", "")
}