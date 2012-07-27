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

package com.wheaties.squealer.generator.scala

import com.wheaties.squealer.db._
import java.util.Date
import com.wheaties.squealer.generator.Formato

object ScalaDocs extends ((Table,Formato) => List[String]){
  def apply(table: Table, formato: Formato) ={
    val date = new Date(System.currentTimeMillis)
    val creation = formato.tableName(table.name) + " was created on " + date.toString

    val comments = for{
      column <- table.columns
      comment <- column.comment
    } yield{ "@" + formato.columnName(column.name) + " " + comment }

    table.comment match{
      case Some(comment) => comment :: "" :: creation :: "" :: comments
      case None => creation :: "" :: comments
    }
  }
}










