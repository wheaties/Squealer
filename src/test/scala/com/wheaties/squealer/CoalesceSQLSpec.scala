package com.wheaties.squealer

import org.specs2.mutable.Specification


class CoalesceSQLSpec {

}

class CoalesceFromClauseSpec extends Specification{
  "substitute" should{
    val mapped = Map(Term("foo", None) -> Table("foo", None, Nil), Term("bar", Some("b")) -> Table("bar", None, Nil))
    val column = ColumnDef("yo", "Int", None, None)
    def makeCol(name: String) ={
      val output = CoalesceFromClause.substitute(name, mapped, _.copy(columns = List(column)))
      for{
        (_, table) <- output
        singleCol <- table.columns
      } yield singleCol
    }
    "substitute a table with a matching name" in{
      makeCol("foo") must contain(column)
    }

    "substitute a table with a matching aliased name" in{
      makeCol("b") must contain(column)
    }

    "substitute nothing if no matching name exists" in{
      makeCol("oh no") must not contain(column)
    }
  }

  "nullColumns" in{
    val table = Table("foo", None, ColumnDef("bar", "Int", None, None) :: Nil)
    val nulled = CoalesceFromClause.nullColumns(table)

    nulled.columns must contain(NullableColumnDef("bar", "Int", None))
  }

  "extractTerms" should{
    val term = Term("foo", None)

    "work with Term" in{
      val out = CoalesceFromClause.extractTerms(term :: Nil)
      out must contain(term)
    }

    "work with LeftJoin" in{
      val out = CoalesceFromClause.extractTerms(LeftJoin(term, term) :: Nil)
      out must contain(term)
    }

    "work with RightJoin" in{
      val out = CoalesceFromClause.extractTerms(RightJoin(term, term) :: Nil)
      out must contain(term)
    }

    "work with Join" in{
      val out = CoalesceFromClause.extractTerms(Join(term, term) :: Nil)
      out must contain(term)
    }

    "work with FullJoin" in{
      val out = CoalesceFromClause.extractTerms(FullJoin(term, term) :: Nil)
      out must contain(term)
    }
  }

  "mapTableAliases" should{
    val terms = Term("foo", None) :: Term("bar", Some("b")) :: Nil
    val tables = Table("foo", None, Nil) :: Table("b", None, Nil) :: Nil
    val mapped = CoalesceFromClause.mapTableAliases(terms, tables)

    "map by table names" in{
      mapped.get(Term("foo", None)) must beSome
    }

    "not map by table aliases" in{
      mapped.get(Term("bar", Some("b"))) must beNone
    }

    "map only found tables" in{
      mapped must have size(1)
    }
  }

  "left joins" should{
    val tables = Table("foo", None, ColumnDef("id", "Int", None, None) :: Nil) ::
      Table("bar", None, ColumnDef("id", "Int", None, None) :: Nil) :: Nil
    val from = From(Term("bar", None) :: LeftJoin(Term("foo", None), Conditional("foo.id", "bar.id")) :: Nil)
    val mapped = CoalesceFromClause(from, tables)

    "produce the columns of both tables joined" in{
      mapped must be size(2)
    }

    "force the right hand side's columns to null" in{
      val nulled = for{
        (_, table) <- mapped if table.name eq "foo"
        column <- table.columns
      } yield column

      nulled must contain(NullableColumnDef("id", "Int", None))
    }
  }
}