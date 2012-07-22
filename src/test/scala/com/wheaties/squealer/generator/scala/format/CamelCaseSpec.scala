package com.wheaties.squealer.generator.scala.format

import org.specs2.mutable.Specification

class CamelCaseSpec extends Specification{
  "camelCase format" should{
    "leave good names unchanged" in{
      CamelCase.format("helloWorld") must be_==("helloWorld")
    }

    "handle snake case" in{
      CamelCase.format("hello_world") must be_==("helloWorld")
    }

    "handle pascal case" in{
      CamelCase.format("HelloWorld") must be_==("helloWorld")
    }

    "handle spaces" in{
      CamelCase.format("hello world") must be_==("helloWorld")
    }
  }

  "camelCase (tables)" should{
    "handle snake case" in{
      CamelCase.tableName("my_name") must be_==("MyName")
    }

    "handle lower case" in{
      CamelCase.tableName("myname") must be_==("Myname")
    }

    "handle name identity" in{
      CamelCase.tableName("Myname") must be_==("Myname")
    }

    "handle column name snake case" in{
      CamelCase.columnName("column_name") must be_==("columnName")
    }

    "handle column name identity" in{
      CamelCase.columnName("columnName") must be_==("columnName")
    }
  }
}