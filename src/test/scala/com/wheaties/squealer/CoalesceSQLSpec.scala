package com.wheaties.squealer

import org.specs2.mutable.Specification


class CoalesceSQLSpec {

}

class CoalesceFromClauseSpec extends Specification{
  "substitute" should{
    val mapped = Map(Term("foo", None) -> Table("foo", None, Nil), Term("bar", Some("b")) -> Table("bar", None, Nil))
    val column = Column("yo", "Int", None, None, ColumnDef)
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

    "substitute a table from a conditional on a join" in{
      makeCol("foo.id") must contain(column)
    }

    "substitute a table from an aliased conditoinal on a join" in{
      makeCol("b.id") must contain(column)
    }

    "substitute nothing if no matching name exists" in{
      makeCol("oh no") must not contain(column)
    }
  }

  "nullColumns" in{
    val table = Table("foo", None, Column("bar", "Int", None, None, ColumnDef) :: Nil)
    val nulled = CoalesceFromClause.nullColumns(table)

    nulled.columns must contain(Column("bar", "Int", None, None, NullableColumn))
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

  "conditionalExists" should{
    val exists = CoalesceFromClause.conditionalExists(Term("foo", Some("bar")) :: Nil)

    "return true for term names" in{
      exists("foo") must beTrue
    }

    "return true for aliased term names" in{
      exists("bar") must beTrue
    }

    "return false for names that don't exist" in{
      exists("foobar") must beFalse
    }
  }

  "left joins" should{
    val tables = Table("foo", None, Column("id", "Int", None, None, ColumnDef) :: Nil) ::
      Table("bar", None, Column("id", "Int", None, None, ColumnDef) :: Nil) :: Nil
    val from = From(Term("bar", None) :: LeftJoin(Term("foo", None), Conditional("foo.id", "bar.id")) :: Nil)
    val mapped = CoalesceFromClause(from, tables)

    "produce the columns of both tables joined" in{
      mapped must be size(2)
    }

    "force the right hand side's columns to null" in{
      val nulled = for{
        (_, table) <- mapped if table.name eq "bar"
        column <- table.columns
      } yield column

      nulled must contain(Column("id", "Int", None, None, NullableColumn))
    }
  }

  "right joins" should{
    val tables = Table("foo", None, Column("id", "Int", None, None, ColumnDef) :: Nil) ::
      Table("bar", None, Column("id", "Int", None, None, ColumnDef) :: Nil) :: Nil
    val from = From(Term("bar", None) :: RightJoin(Term("foo", None), Conditional("foo.id", "bar.id")) :: Nil)
    val mapped = CoalesceFromClause(from, tables)

    "produce the columns of both tables joined" in{
      mapped must be size(2)
    }

    "force the left hand side's columns to null" in{
      val nulled = for{
        (_, table) <- mapped if table.name eq "foo"
        column <- table.columns
      } yield column

      nulled must contain(Column("id", "Int", None, None, NullableColumn))
    }
  }

  "full joins" should{
    val tables = Table("foo", None, Column("id", "Int", None, None, ColumnDef) :: Nil) ::
      Table("bar", None, Column("id", "Int", None, None, ColumnDef) :: Nil) :: Nil
    val from = From(Term("bar", None) :: FullJoin(Term("foo", None), Conditional("foo.id", "bar.id")) :: Nil)
    val mapped = CoalesceFromClause(from, tables)

    "produce the columns of both tables joined" in{
      mapped must be size(2)
    }

    "force the both table's columns to null" in{
      val nulled = for{
        (_, table) <- mapped
        column <- table.columns
      } yield column

      nulled count(_ == Column("id", "Int", None, None, NullableColumn)) must be_==(2)
    }
  }
}

class CoalesceSelectClauseSpec extends Specification{
  "findColumn" should{
    val fooCol = Column("one", "Double", None, None, ColumnDef)
    val barCol = Column("two", "Double", None, None, ColumnDef)
    val mapped = Map(Term("foo", None) -> Table("foo", None, fooCol :: Nil),
      Term("bar", Some("b")) -> Table("bar", None, barCol :: Nil))

    val findColumn = CoalesceSelectClause.findColumn(mapped)

    "return a function that identifies columns by name" in{
      findColumn("foo.one") must be_==(Some(fooCol))
    }

    "return a function that identifies columns by alias" in{
      findColumn("b.two") must be_==(Some(barCol))
    }

    "not identify the right column from the wrong table" in{
      findColumn("bar.one") must beNone
    }
  }

  "CoalesceSelectClause" should{
    val fooCol = Column("id", "Int", None, None, ColumnDef)
    val tableMap = Map(Term("foo", None) -> Table("foo", None, fooCol :: Nil))

    "handle selects where the tables are present" in{
      val select = Select(Term("id", None) :: Nil)

      CoalesceSelectClause(select, tableMap) must contain(fooCol)
    }

    "rename columns if aliased" in{
      val select = Select(Term("id", Some("identity")) :: Nil)

      CoalesceSelectClause(select, tableMap) must contain(fooCol.copy(name = "identity"))
    }

    "work with wildcards" in{
      val select = Select(Wildcard :: Nil)

      CoalesceSelectClause(select, tableMap) must contain(fooCol)
    }

    "work with Counts" in{
      val select = Select(Count("id", Some("sum")) :: Nil)

      CoalesceSelectClause(select, tableMap) must contain(Column("sum", "Int", None, None, ColumnDef))
    }
  }
}