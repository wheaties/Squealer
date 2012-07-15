package com.wheaties.squealer.generator.scala.squeryl

import org.specs2.mutable.Specification
import treehugger.forest._
import definitions._
import treehuggerDSL._
import com.wheaties.squealer.db._

class ConstructorTreeSpec extends Specification{
  "keyEntity" should{
    "handle a single primary key" in{
      val columns = Column("bar", IntType, Some("1"), None, PrimaryKey) :: Nil
      val tree = ConstructorTree.keyEntity(columns)
      treeToString(tree) must be_==("KeyedEntity[Int]")
    }

    "handle multiple primary keys" in{
      val column = Column("bar", IntType, Some("1"), None, PrimaryKey)
      val tree = ConstructorTree.keyEntity(column :: column :: Nil)
      treeToString(tree) must be_==("KeyedEntity[CompositeKey2[Int, Int]]")
    }
  }

  "apply and indirectly args" should{
    "create default values" in{
      val columns = Column("bar", IntType, Some("1"), None, ColumnDef) :: Nil
      val tree = ConstructorTree("Foo", columns, identity)
      treeToString(tree.tree) must be_==("case class Foo(@Column(\"bar\") bar: Int = 1)")
    }
    "handle nullable types" in{
      val columns = Column("bar", IntType, None, None, NullableColumn) :: Nil
      val tree = ConstructorTree("Foo", columns, identity)
      treeToString(tree.tree) must be_==("case class Foo(@Column(\"bar\") bar: Option[Int])")
    }
    "make a class constructor for more than 22 columns" in{
      val columns = for{indx <- 0 to 22} yield Column("bar" + indx.toString, IntType, None, None, ColumnDef)
      val tree = ConstructorTree("Foo", columns.toList, identity)
      treeToString(tree.tree) must be_==("class Foo(val @Column(\"bar0\") bar0: Int, val @Column(\"bar1\") bar1: Int, " +
        "val @Column(\"bar2\") bar2: Int, val @Column(\"bar3\") bar3: Int, val @Column(\"bar4\") bar4: Int, val @Column(\"bar5\") bar5: Int, " +
        "val @Column(\"bar6\") bar6: Int, val @Column(\"bar7\") bar7: Int, val @Column(\"bar8\") bar8: Int, val @Column(\"bar9\") bar9: Int, " +
        "val @Column(\"bar10\") bar10: Int, val @Column(\"bar11\") bar11: Int, val @Column(\"bar12\") bar12: Int, val @Column(\"bar13\") bar13: Int, " +
        "val @Column(\"bar14\") bar14: Int, val @Column(\"bar15\") bar15: Int, val @Column(\"bar16\") bar16: Int, val @Column(\"bar17\") bar17: Int, " +
        "val @Column(\"bar18\") bar18: Int, val @Column(\"bar19\") bar19: Int, val @Column(\"bar20\") bar20: Int, val @Column(\"bar21\") bar21: Int, " +
        "val @Column(\"bar22\") bar22: Int)")
    }
  }
}

class DefinitionTreeSpec extends Specification{
  "id" should{
    "handle nullable primary keys" in{
      val columns = Column("bar", IntType, None, None, NullablePrimaryKey) :: Nil
      val tree = DefinitionsTree.id(columns, identity)
      treeToString(tree) must be_==("def id = compositeKey(bar)")
    }
    "handle primary keys" in{
      val columns = Column("bar", IntType, None, None, PrimaryKey) :: Nil
      val tree = DefinitionsTree.id(columns, identity)
      treeToString(tree) must be_==("def id = compositeKey(bar)")
    }
    "handle multiple keys" in{
      val columns = Column("foo", IntType, None, None, NullablePrimaryKey) ::
        Column("bar", IntType, None, None, PrimaryKey) ::
        Column("baz", IntType, None, None, PrimaryKey) :: Nil
      val tree = DefinitionsTree.id(columns, identity)
      treeToString(tree) must be_==("def id = compositeKey(foo, bar, baz)")
    }
  }

  "optionConstruction" should{
    "handle nullable fields with a default" in{
      val columns = Column("foo", IntType, Some("1"), None, NullablePrimaryKey) :: Nil
      val tree = DefinitionsTree.optionConstructor(columns)
      treeToString(tree) must be_==("def this() = this(Some(1))")
    }
    "handle non-nullable fields with a default" in{
      val columns = Column("foo", IntType, Some("1"), None, ColumnDef) :: Nil
      val tree = DefinitionsTree.optionConstructor(columns)
      treeToString(tree) must be_==("def this() = this(1)")
    }
    "handle nullable fields without a default" in{
      val columns = Column("foo", IntType, None, None, NullableColumn) :: Nil
      val tree = DefinitionsTree.optionConstructor(columns)
      treeToString(tree) must be_==("def this() = this(Some(0))")
    }
    "handle nullable fields without a default" in{
      val columns = Column("foo", IntType, None, None, ColumnDef) :: Nil
      val tree = DefinitionsTree.optionConstructor(columns)
      treeToString(tree) must be_==("def this() = this(0)")
    }
    "multiple fields" in{
      val columns = Column("foo", IntType, Some("1"), None, ColumnDef) ::
        Column("bar", IntType, None, None, ColumnDef) ::
        Column("baz", IntType, None, None, NullableColumn) :: Nil
      val tree = DefinitionsTree.optionConstructor(columns)
      treeToString(tree) must be_==("def this() = this(1, 0, Some(0)")
    }
  }

  "defaultArg" should{
    "do byte arrays" in{
      val tree = DefinitionsTree.defaultArg(BinaryType)
      treeToString(tree) must be_==("new Array[Byte]")
    }
    "do Blob" in{
      val tree = DefinitionsTree.defaultArg(BlobType)
      treeToString(tree) must be_==("new SerialBlob(new Array[Byte])")
    }
    "do Clob" in{
      val tree = DefinitionsTree.defaultArg(ClobType)
      treeToString(tree) must be_==("new SerialClob(new Array[Char])")
    }
  }
}