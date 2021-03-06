package com.wheaties.squealer.generator.scala.jdbc

import com.wheaties.squealer.db._
import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class ConstructorTreeSpec extends Specification{

  "Table without columns" should{
    val tree = ConstructorTree("yo", Nil)

    "make an empty constructor list" in{
      treeToString(tree.tree) must be_==("case class yo")
    }
  }

  "Table with less than 22 columns" should{

    "make a non-empty constructor list" in{
      val tree = ConstructorTree("Yo", Column("hey", StringType, None, None, ColumnDef) :: Nil)
      treeToString(tree.tree) must be_==("case class Yo(hey: String)")
    }

    "handle default arguments" in{
      val tree = ConstructorTree("Yo", Column("hey", StringType, Some("Hello"), None, ColumnDef) :: Nil)
      treeToString(tree.tree) must be_==("case class Yo(hey: String = \"Hello\")")
    }
  }

  "Table with more than 22 columns" should{
    val chars = "abcdefghijklmnopqrstuvwxyz".toCharArray
    val columns = for{indx <- 0 to 22} yield Column(chars(indx).toString, IntType, None, None, ColumnDef)

    "make a non-empty constructor list" in{
      val tree = ConstructorTree("Yo", columns.toList)
      treeToString(tree.tree) must be_==("class Yo(val a: Int, val b: Int, val c: Int, val d: Int, val e: Int, val f: Int, val g: Int, val h: Int, val i: Int, val j: Int, val k: Int, val l: Int, val m: Int, val n: Int, val o: Int, val p: Int, val q: Int, val r: Int, val s: Int, val t: Int, val u: Int, val v: Int, val w: Int)")
    }
  }
}

class AssumptionTreeSpec extends Specification{

  "Table with a sized column" should{
    val column = new Column("Foo", IntType, None, None, ColumnDef) with WithSize{
      override val size = 42
      override val precision = 0
    }

    "create an assumption" in{
      val tree = AssumptionTree.makeAssumption(column, "Bar")
      treeToString(tree) must be_==("assume(Bar.size <= 42, \"Bar.size must be less than 42\")")
    }

    "pass the check" in{
      AssumptionTree.check(column) must beTrue
    }
  }

  "Table with a length column" should{
    val column = new Column("Foo", IntType, None, None, ColumnDef) with WithLength{
      override val length = 42
    }

    "create an assumption" in{
      val tree = AssumptionTree.makeAssumption(column, "Bar")
      treeToString(tree) must be_==("assume(Bar.length <= 42, \"Bar.length must be less than 42\")")
    }

    "pass the check" in{
      AssumptionTree.check(column) must beTrue
    }
  }

  "Table with a numeric scaled column" should{
    val column = new Column("Foo", IntType, None, None, ColumnDef) with WithScale{
      override val precision = 42
      override val scale = 42
    }

    "create an assumption" in{
      val tree = AssumptionTree.makeAssumption(column, "Bar")
      treeToString(tree) must be_==("assume((Bar.precision <= 42) && (Bar.scale <= 42), \"Bar.precision must be less than 42\")")
    }

    "pass the check" in{
      AssumptionTree.check(column) must beTrue
    }
  }

  "Table with several columns" should{
     val column = new Column("Foo", IntType, None, None, ColumnDef) with WithSize{
      override val size = 42
    }

    "create an assumption" in{
      val trees = AssumptionTree(column :: column :: Nil)
      trees.forall(tree => treeToString(tree) must be_==("assume(Foo.size <= 42, \"Foo.size must be less than 42\")"))
    }
  }
}

class EqualsTreeSpec extends Specification{

  "Table with less than 22 columns and no Primary Keys" should{
    "return an empty tree for no columns" in{
      EqualsTree("Foo", Nil) must be_==(EmptyTree)
    }

    "return an empty tree for less than 23 columns" in{
      EqualsTree("Foo", Column("b", IntType, None, None, ColumnDef) :: Nil) must be_==(EmptyTree)
    }
  }

  "Table with One Primary Key" should{
    val keys = Column("a", IntType, None, None, PrimaryKey) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a) => this.a == a\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys)
      treeToString(tree) must be_==("List(Ident(a))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.withKeys(keys)
      treeToString(tree) must be_==("this.a == a")
    }
  }

  "Table with a composite Primary Key" should{
    val keys = Column("a", IntType, None, None, PrimaryKey) :: Column("b", IntType, None, None, PrimaryKey) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a, b) => this.b == b && (this.a == a)\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys)
      treeToString(tree) must be_==("List(Ident(a), Ident(b))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.withKeys(keys)
      treeToString(tree) must be_==("this.b == b && (this.a == a)")
    }
  }

  "Table with mixed Column types" should{
    val keys = Column("a", IntType, None, None, PrimaryKey) :: Column("b", IntType, None, None, ColumnDef) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a, _) => this.a == a\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys)
      treeToString(tree) must be_==("List(Ident(a), Ident(_))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.withKeys(keys)
      treeToString(tree) must be_==("this.a == a")
    }
  }

  "Table with more than 22 columns and no primary keys" should{
    val columns = for{i <- 0 to 22} yield Column("a", IntType, None, None, ColumnDef)

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", columns.toList)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case (x: Foo) => this.a == x.a && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a) && (this.a == x.a)\n    case _ => false\n  }")
    }
  }
}

class HashCodeTreeSpec extends Specification{

  "Table with less than 23 columns but no Primary Keys" should{
    val noKeys = Column("a", IntType, None, None, ColumnDef) :: Nil

    "produce an empty tree" in{
      HashCodeTree(noKeys) must be_==(EmptyTree)
    }
  }

  "Table with one Primary Key" should{
    val keys = Column("a", IntType, None, None, PrimaryKey) :: Nil

    "create a proper hashCode method" in{
      val tree = HashCodeTree(keys)
      treeToString(tree) must be_==("override def hashCode = a.hashCode")
    }
  }

  "Table with a composite Primary Key" should{
    val keys = Column("a", IntType, None, None, PrimaryKey) :: Column("b", IntType, None, None, PrimaryKey) :: Nil

    "create a proper hashCode method" in{
      val tree = HashCodeTree(keys)
      treeToString(tree) must be_==("override def hashCode = List(b.hashCode, a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)")
    }
  }

  "Table with more than 22 columns but no Primary Key" should{
    val columns = for{i <- 0 to 22} yield Column("a", IntType, None, None, ColumnDef)
    val tree = HashCodeTree(columns.toList)
    treeToString(tree) must be_==("override lazy val hashCode = List(a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode, a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)")
  }
}

class CopyTreeSpec extends Specification{

  "Table" should{
    val column = Column("a", IntType, None, None, ColumnDef)

    "generate an empty tree for less than 23 columns" in{
      CopyTree("Foo", column :: Nil) must be_==(EmptyTree)
    }

    "generate multiple arguments with multiple columns" in{
      val columns = for{i <- 0 to 22} yield column
      val tree = CopyTree("Foo", columns.toList)
      treeToString(tree) must be_==("def copy(a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a, a: Int = this.a) = new Foo(a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)")
    }
  }
}
