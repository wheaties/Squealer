package com.wheaties.squealer

import org.specs2.mutable.Specification
import com.typesafe.config.{ConfigFactory, ConfigResolveOptions, ConfigMergeable, Config}

/**
 * More of an integration test, no?
 */
class ConfigParserSpec extends Specification{
  "parse tables" should{

    "find no tables where none exist" in{
      val config = ConfigFactory.load("group_test.conf")
      val output = ConfigParser.parseTables(config)
      output must beEmpty
    }

    "find a table where one exists" in{
      val config = ConfigFactory.load("test.conf")
      val output = ConfigParser.parseTables(config)
      output must haveSize(1)
    }
  }

  "parse queries" should{

    "find no queries where none exist" in{
      val config = ConfigFactory.load("test.conf")
      val output = ConfigParser.parseQueries(config)
      output must beEmpty
    }

    "find a query where one exists" in{
      val config = ConfigFactory.load("group_test.conf")
      val output = ConfigParser.parseQueries(config)
      output must haveSize(1)
    }
  }
}