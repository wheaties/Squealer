package com.wheaties.squealer

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
    def table(name: String, columnName: String = "yo") = Table(name, None, ColumnDef(columnName, "Int", None, None) :: Nil)

    "handle snake case" in{
      CamelCase(table("my_name")).name must be_==("MyName")
    }

    "handle lower case" in{
      CamelCase(table("myname")).name must be_==("Myname")
    }

    "handle name identity" in{
      CamelCase(table("Myname")).name must be_==("Myname")
    }

    "handle column name snake case" in{
      CamelCase(table("name", "column_name")).columns.head.name must be_==("columnName")
    }

    "handle column name identity" in{
      CamelCase(table("name", "columnName")).columns.head.name must be_==("columnName")
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