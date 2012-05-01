package com.wheaties.squealer

import org.specs2.mutable.Specification


class CoalesceSQLSpec {

}

class CoalesceFromClauseSpec extends Specification{
  "substitute" should{
    val mapped = Map(Term("foo", None) -> Table("foo", None, Nil), Term("bar", Some("b")) -> Table("bar", None, Nil))
    "substitute a table with a matching name" in{
      val output = CoalesceFromClause.substitute("foo", mapped, _.copy(columns = List(ColumnDef("yo", "Int", None, None))))
      val column = for{
        (_, table) <- output
        singleCol <- table.columns
      } yield singleCol
      column must contain(ColumnDef("yo", "Int", None, None))
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
        (term, table) <- mapped if table.name eq "foo"
        column <- table.columns if column.name eq "id"
      } yield column

      nulled must contain(NullableColumnDef("id", "Int", None))
    }
  }
}