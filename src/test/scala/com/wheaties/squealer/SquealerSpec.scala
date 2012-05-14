package com.wheaties.squealer

import org.specs2.mutable.Specification
import treehugger.forest._
import definitions._
import treehuggerDSL._

class SquealerSpec extends Specification{
  val squeal = new Object with Squealer

  class SideEffectTester extends Recorder[ParsedResult]{
    val builder = new StringBuilder

    def record(result: ParsedResult){
      builder ++= treeToString(result.ast)
    }
  }
}