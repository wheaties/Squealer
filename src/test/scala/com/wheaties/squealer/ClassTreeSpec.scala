package com.wheaties.squealer

import treehugger.forest._
import definitions._
import treehuggerDSL._
import org.specs2.mutable._

class ConstructorTreeSpec extends Specification{

  "Table without columns" should{
    val tree = ConstructorTree("yo", Nil)

    "make an empty constructor list" in{
      treeToString(tree) must be_==("case class yo")
    }
  }

  "Table with less than 22 columns" should{

    "make a non-empty constructor list" in{
      val tree = ConstructorTree("Yo", ColumnDef("hey", "String", None) :: Nil)
      treeToString(tree) must be_==("case class Yo(hey: String)")
    }

    "handle default arguments" in{
      val tree = ConstructorTree("Yo", ColumnDef("hey", "String", Some("Hello")) :: Nil)
      treeToString(tree) must be_==("case class Yo(hey: String = \"Hello\")")
    }
  }

  "Table with more than 22 columns" should{
    val chars = "abcdefghijklmnopqrstuvwxyz".toCharArray
    val columns = for{indx <- 0 to 22} yield ColumnDef(chars(indx).toString, "Int", None)

    "make a non-empty constructor list" in{
      val tree = ConstructorTree("Yo", columns.toList)
      treeToString(tree) must be_==("class Yo(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int, u: Int, v: Int, w: Int)")
    }
  }
}

class AssumptionTreeSpec extends Specification{

  "Table with a sized column" should{
    val column = new ColumnDef("Foo", "Int", None) with WithSize{
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
    val column = new ColumnDef("Foo", "Int", None) with WithLength{
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
    val column = new ColumnDef("Foo", "Int", None) with WithScale{
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
     val column = new ColumnDef("Foo", "Int", None) with WithSize{
      override val size = 42
    }

    "create an assumption" in{
      val trees = AssumptionTree(column :: column :: Nil)
      trees.forall(tree => treeToString(tree) must be_==("assume(Foo.size <= 42, \"Foo.size must be less than 42\")"))
    }
  }
}

class EqualsTreeSpec extends Specification{

  "Table without Primary Keys" should{
    val noKeys = ColumnDef("a", "Int", None) :: Nil

    "throw an exception at runtime" in{
      EqualsTree("Foo", noKeys) must throwA[UnsupportedOperationException]
    }

    "create an empty list of pattern matching args" in{
      val tree = EqualsTree.pattern(noKeys, Nil)
      treeToString(tree) must be_==("List(Ident(_))")
    }

    "throw an exception at runtime when envoking make" in{
      EqualsTree.make(noKeys, Nil) must throwA[UnsupportedOperationException]
    }
  }

  "Table with One Primary Key" should{
    val keys = PrimaryKeyDef("a", "Int", None) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a) => this.a eq that.a\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys, Nil)
      treeToString(tree) must be_==("List(Ident(a))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.make(keys, Nil)
      treeToString(tree) must be_==("this.a eq that.a")
    }
  }

  "Table with a composite Primary Key" should{
    val keys = PrimaryKeyDef("a", "Int", None) :: PrimaryKeyDef("b", "Int", None) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a, b) => (this.b eq that.b) && (this.a eq that.a)\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys, Nil)
      treeToString(tree) must be_==("List(Ident(a), Ident(b))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.make(keys, Nil)
      treeToString(tree) must be_==("(this.b eq that.b) && (this.a eq that.a)")
    }
  }

  "Table with mixed Column types" should{
    val keys = PrimaryKeyDef("a", "Int", None) :: ColumnDef("b", "Int", None) :: Nil

    "create a proper equals method" in{
      val tree = EqualsTree("Foo", keys)
      treeToString(tree) must be_==("override def equals(that: Any) =\n  that match {\n    case Foo(a, _) => this.a eq that.a\n    case _ => false\n  }")
    }

    "create a list of one pattern matching argument" in{
      val tree = EqualsTree.pattern(keys, Nil)
      treeToString(tree) must be_==("List(Ident(a), Ident(_))")
    }

    "create a list of one check" in{
      val tree = EqualsTree.make(keys, Nil)
      treeToString(tree) must be_==("this.a eq that.a")
    }
  }
}

class HashCodeTreeSpec extends Specification{

  "Table with no Primary Keys" should{
    val noKeys = ColumnDef("a", "Int", None) :: Nil

    "throw an exception" in{
      HashCodeTree(noKeys) must throwA[UnsupportedOperationException]
    }
  }

  "Table with one Primary Key" should{
    val keys = PrimaryKeyDef("a", "Int", None) :: Nil

    "create a proper hashCode method" in{
      val tree = HashCodeTree(keys)
      treeToString(tree) must be_==("override def hashCode = List(a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)")
    }
  }

  "Table with a composite Primary Key" should{
    val keys = PrimaryKeyDef("a", "Int", None) :: PrimaryKeyDef("b", "Int", None) :: Nil

    "create a proper hashCode method" in{
      val tree = HashCodeTree(keys)
      treeToString(tree) must be_==("override def hashCode = List(b.hashCode, a.hashCode).reduceLeft((left, right) => (left * 17) ^ right)")
    }
  }
}

class CopyTreeSpec extends Specification{

  "Table" should{
    val column = ColumnDef("a", "Int", None)

    "generate a single argument copy with one column" in{
      val tree = CopyTree("Foo", column :: Nil)
      treeToString(tree) must be_==("def copy(a: Int = this.a) = new Foo(a)")
    }

    "generate multiple arguments with multiple columns" in{
      val tree = CopyTree("Foo", column :: column :: Nil)
      treeToString(tree) must be_==("def copy(a: Int = this.a, a: Int = this.a) = new Foo(a, a)")
    }
  }
}
