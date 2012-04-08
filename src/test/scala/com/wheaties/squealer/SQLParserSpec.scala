package com.wheaties.squealer

import org.specs2.mutable.Specification
import com.codecommit.gll._

class SQLParserSpec extends Specification{
  "variable" should{

    //TODO: get rid of the ambiguity
    "handle wilcard chars" in{
      val output = SQLParser.variable("*")
      output.length must be_==(2)
      output must contain(Success(Term("*", None), LineNil))
      output must contain(Success(Wildcard, LineNil))
    }

    "handle an aliased term" in{
      val output = SQLParser.variable("foo as foobar")
      output.length must be_==(1)
      output must contain(Success(Term("foo", Some("foobar")), LineNil))
    }

    "handle a non-aliased term" in{
      val output = SQLParser.variable("foo")
      output.length must be_==(1)
      output must contain(Success(Term("foo", None), LineNil))
    }
  }

  "terms" should{
    "handle single fields" in{
      val output = SQLParser.terms("foo")
      output must contain(Success(List(Term("foo", None)), LineNil))
    }

    "handle multiple fields" in{
      val output = SQLParser.terms("foo, bar")
      output must contain(Success(List(Term("foo", None), Term("bar", None)), LineNil))
    }

    "handle multiple fields with alias" in{
      val output = SQLParser.terms("foo, bar as yo")
      output must contain(Success(List(Term("foo", None), Term("bar", Some("yo"))), LineNil))
    }
  }

  "select" should{
    "work with wildcards" in{
      val output = SQLParser.select("SELECT *")
      output must contain(Success(Select(List(Wildcard)), LineNil))
    }

    "work with multiple fields" in{
      val output = SQLParser.select("SELECT foo, bar")
      output must contain(Success(Select(List(Term("foo", None), Term("bar", None))), LineNil))
    }

    "work with multiple fields with alias" in{
      val output = SQLParser.select("SELECT foo, bar AS lalala")
      output must contain(Success(Select(List(Term("foo", None), Term("bar", Some("lalala")))), LineNil))
    }
  }
}