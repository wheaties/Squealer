package com.wheaties.squealer.generator

import com.wheaties.squealer.db._
import org.specs2.mutable.Specification

class FormatoSpec extends Specification{
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

  "regex format" should{
    "replace digits with nothing" in{
      val regex = new RegexFormato("[\\d]", "")
      regex.format("foo21bar") must be_==("foobar")
    }

    "replace foo with bar" in{
      val regex = new RegexFormato("foo", "bar")
      regex.format("foobar") must be_==("barbar")
    }
  }
}