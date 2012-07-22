package com.wheaties.squealer.generator.scala.format

import org.specs2.mutable.Specification

class RegexFormatoSpec extends Specification{
  "regex format" should{
    "replace digits with nothing" in{
      val regex = new RegexFormato("[\\d]", "")
      regex.format("foo21bar") must be_==("foobar")
    }

    "replace foo with bar" in{
      val regex = new RegexFormato("foo", "bar")
      regex.format("foobar") must be_==("barbar")
    }
  }
}