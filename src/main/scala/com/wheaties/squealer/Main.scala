/*
Copyright (c) 2012 Owein Reese

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.wheaties.squealer

import config._
import db._
import generator.scala.ScalaGenerator

object Main extends Squealer{
  def main(args: Array[String]):Unit = try{
    val exceptions = if(args.isEmpty){
      action(ConfigParser("squealer.conf"))
    }
    else{
      args map(ConfigParser) flatMap(action) toList
    }

    exceptions foreach println
  }
  catch{
    case ex:Exception => println(ex)
  }
}

trait Squealer{

  def action(params: ConfigParams) ={
    val ConfigParams(tables, db, format) = params
    val DatabaseParams(url, user, password, driver) = db

    val source = ParseDataSource(url, user, password, driver)
    val database = DatabaseTableLens modify(source, _.filter(tables.names contains))
    val generator = ScalaGenerator(format.library, format.regex, format.replacement)

    generator(database, tables.pack)
  }
}