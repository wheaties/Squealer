package com.wheaties.squealer

import org.specs2.mutable.Specification
import com.typesafe.config.ConfigFactory

/**
 * More of an integration test, no?
 */
class ConfigParserSpec extends Specification{
  val loader = getClass.getClassLoader
  val testConf = ConfigFactory.parseResources(loader, "/test.conf")
  val groupConf = ConfigFactory.parseResources(loader, "/group_test.conf")

  "parse tables" should{

    "find no tables where none exist" in{
      val output = ConfigParser.parseTables(groupConf)
      output must beEmpty
    }

    "find a table where one exists" in{
      val output = ConfigParser.parseTables(testConf)
      output must haveSize(1)
    }
  }

  "parse queries" should{

    "find no queries where none exist" in{
      val output = ConfigParser.parseQueries(testConf)
      output must beEmpty
    }

    "find a query where one exists" in{
      val output = ConfigParser.parseQueries(groupConf)
      output must haveSize(1)
    }
  }
}