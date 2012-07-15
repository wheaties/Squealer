package com.wheaties.squealer.generator.scala.squeryl

import org.specs2.mutable.Specification
import treehugger.forest._
import definitions._
import treehuggerDSL._

class SchemaTreeSpec extends Specification{
  def format(in: String) = "a" + in

  "tableDef" should{
    "do stuff" in{
      val tree = SchemaTree.tableDef("turkey", format)
      treeToString(tree) must be_==("val aturkey = table[Aturkey](\"turkey\")")
    }
  }

  "apply" should{
    "do stuff" in{
      val tree = SchemaTree("foo", List("bar"), format)
      treeToString(tree) must be_==("object Afoo extends Schema {\n  val abar = table[Abar](\"bar\")\n}")
    }
  }
}