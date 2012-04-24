package com.wheaties.squealer

import org.specs2.mutable.Specification
import com.codecommit.gll._

class SQLParserSpec extends Specification{

  "aliased" should{
    "handle an aliased term" in{
      val output = SQLParser.aliased("foo as foobar")
      output.length must be_==(1)
      output must contain(Success(Term("foo", Some("foobar")), LineNil))
    }

    "handle a non-aliased term" in{
      val output = SQLParser.aliased("foo")
      output.length must be_==(1)
      output must contain(Success(Term("foo", None), LineNil))
    }
  }

  "terms" should{
    //TODO: get rid of the ambiguity
    "handle wilcard chars" in{
      val output = SQLParser.terms("*")
      output.length must be_==(2)
      output must contain(Success(List(Term("*", None)), LineNil))
      output must contain(Success(List(Wildcard), LineNil))
    }

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

  "chained" should{
    "work with single item lists" in{
      val output = SQLParser.chained("foo")
      output must contain(Success(List("foo"), LineNil))
    }

    "work with multi item lists" in{
      val output = SQLParser.chained("foo, bar")
      output must contain(Success(List("foo", "bar"), LineNil))
    }
  }

  "equation" should{
    "compute less than" in{
      val output = SQLParser.equation("c.foo < p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }

    "compute less than or equal to" in{
      val output = SQLParser.equation("c.foo <= p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }

    "compute greater than" in{
      val output = SQLParser.equation("c.foo > p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }

    "compute greater than or equal to" in{
      val output = SQLParser.equation("c.foo >= p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }

    "compute equal to" in{
      val output = SQLParser.equation("c.foo = p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }

    "compute not equal to" in{
      val output = SQLParser.equation("c.foo <> p.bar")
      output must contain(Success(Conditional("c.foo", "p.bar"), LineNil))
    }
  }

  "like" should{
    "just work" in{
      val output = SQLParser.like("foo like 'yo'")
      output must contain(Success(Like, LineNil))
    }
  }

  "between" should{
    "just work" in{
      val output = SQLParser.between("foo between 8 and 9")
      output must contain(Success(InBetween("foo", List("8", "9")), LineNil))
    }
  }

  "in" should{
    "work with single lists" in{
      val output = SQLParser.in("foo in (9)")
      output must contain(Success(InBetween("foo", List("9")), LineNil))
    }

    "work with lists" in{
      val output = SQLParser.in("foo in(9, 8, 7)")
      output must contain(Success(InBetween("foo", List("9", "8", "7")), LineNil))
    }
  }

  "chainedExpresion" should{
    "work with single expressions" in{
      val output = SQLParser.chainExpression("ff in (8,9)")
      output must contain(Success(List(InBetween("ff", List("8", "9"))), LineNil))
    }

    "work with 'or'" in{
      val output = SQLParser.chainExpression("c.id = 1 or c.id = 2")
      output must contain(Success(List(Conditional("c.id", "1"), Conditional("c.id", "2")), LineNil))
    }

    "work with 'and'" in{
      val output = SQLParser.chainExpression("c.id = 1 AND c.id = 2")
      output must contain(Success(List(Conditional("c.id", "1"), Conditional("c.id", "2")), LineNil))
    }

    "work with a combination of 'and' and 'or'" in{
      val output = SQLParser.chainExpression("c.id = 1 AND c.id = 2 OR c.id < 0")
      output must contain(Success(List(Conditional("c.id", "1"), Conditional("c.id", "2"), Conditional("c.id", "0")), LineNil))
    }
  }

  "where" should{
    "handle an empty clause" in{
      val output = SQLParser.where("")
      output must contain(Success(EmptyWhere, LineNil))
    }

    "handle an expression" in{
      val output = SQLParser.where("where foo = 1")
      output must contain(Success(Where(List(Conditional("foo", "1"))), LineNil))
    }
  }

  "join" should{
    "work with simple joins" in{
      val output = SQLParser.join("join foo on c.foo = p.foo")
      output must contain(Success(Join(Term("foo", None), Conditional("c.foo", "p.foo")), LineNil))
    }

    "work with simple joins and aliases" in{
      val output = SQLParser.join("join foo as yo on c.foo = yo.foo")
      output must contain(Success(Join(Term("foo", Some("yo")), Conditional("c.foo", "yo.foo")), LineNil))
    }

    "work with inner join" in{
      val output = SQLParser.join("inner join foo on c.foo = p.foo")
      output must contain(Success(Join(Term("foo", None), Conditional("c.foo", "p.foo")), LineNil))
    }

    "work with left join" in{
      val output = SQLParser.join("left join foo on c.foo = p.foo")
      output must contain(Success(LeftJoin(Term("foo", None), Conditional("c.foo", "p.foo")), LineNil))
    }

    "work with right join" in{
      val output = SQLParser.join("right join foo on c.foo = p.foo")
      output must contain(Success(RightJoin(Term("foo", None), Conditional("c.foo", "p.foo")), LineNil))
    }

    "work with full join" in{
      val output = SQLParser.join("full join foo on c.foo = p.foo")
      output must contain(Success(FullJoin(Term("foo", None), Conditional("c.foo", "p.foo")), LineNil))
    }
  }

  "distinct" should{
    "just work" in{
      val output = SQLParser.distinct("distinct(foo)")
      output must contain(Success(Term("foo", None), LineNil))
    }

    "handle aliases" in{
      val output = SQLParser.distinct("distinct(foo) as yo")
      output must contain(Success(Term("foo", Some("yo")), LineNil))
    }
  }

  "count" should{
    "just work" in{
      val output = SQLParser.count("count(foo)")
      output must contain(Success(Count("foo", None), LineNil))
    }

    "handle aliases" in{
      val output = SQLParser.count("count(foo) as yo")
      output must contain(Success(Count("foo", Some("yo")), LineNil))
    }
  }

  "from" should{
    "handle plain from" in{
      SQLParser.from("from foo") must contain(Success(From(List(Term("foo", None))), LineNil))
    }

    "handle joins" in{
      val output = SQLParser.from("from foo\njoin bar on foo.a = bar.a")
      output must contain(Success(From(List(
        Term("foo", None),
        Join(Term("bar", None), Conditional("foo.a", "bar.a")))), LineNil))
    }

    "handle left joins" in{
      val output = SQLParser.from("from foo\nleft join bar on foo.a = bar.a")
      output must contain(Success(From(List(
        Term("foo", None),
        LeftJoin(Term("bar", None), Conditional("foo.a", "bar.a")))), LineNil))
    }

    "handle right joins" in{
      val output = SQLParser.from("from foo\nright join bar on foo.a = bar.a")
      output must contain(Success(From(List(
        Term("foo", None),
        RightJoin(Term("bar", None), Conditional("foo.a", "bar.a")))), LineNil))
    }

    "handle inner joins" in{
      val output = SQLParser.from("from foo\ninner join bar on foo.a = bar.a")
      output must contain(Success(From(List(
        Term("foo", None),
        Join(Term("bar", None), Conditional("foo.a", "bar.a")))), LineNil))
    }

    "handle full joins" in{
      val output = SQLParser.from("from foo\nfull join bar on foo.a = bar.a")
      output must contain(Success(From(List(
        Term("foo", None),
        FullJoin(Term("bar", None), Conditional("foo.a", "bar.a")))), LineNil))
    }
  }

  "union" should{
    "handle a single union" in{
      val output = SQLParser.union("select id from foo union select id from bar")
      output must contain(Success(
        Union(
          SQL(Select(List(Term("id", None))), From(List(Term("foo", None))), EmptyWhere),
          SQL(Select(List(Term("id", None))), From(List(Term("bar", None))), EmptyWhere)),
        LineNil))
    }

    "handle multiple unions" in{
      val output = SQLParser.union("select id from foo union select id from bar union select id from baz")
      output must contain(Success(
        Union(
          SQL(Select(List(Term("id", None))), From(List(Term("foo", None))), EmptyWhere),
          Union(
            SQL(Select(List(Term("id", None))), From(List(Term("bar", None))), EmptyWhere),
            SQL(Select(List(Term("id", None))), From(List(Term("baz", None))), EmptyWhere))),
        LineNil))
    }
  }
}