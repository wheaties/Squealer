package com.wheaties

import treehugger.forest._
import java.io.{FileWriter, File}

package squealer{

  //Think if I should really make a Result[Tree], Result[String], Result[Table]...
  case class ParsedResult(classPackage: String, className: String, ast: Tree)

  trait Recorder[T]{
    def record(result: ParsedResult)
  }

  object Implicits{

    implicit object FileRecorder extends Recorder[ParsedResult]{
      def record(result: ParsedResult) ={
        val ParsedResult(classPackage, className, ast) = result
        val path = classPackage.replace('.', File.pathSeparatorChar)
        val file = new File(path + File.pathSeparatorChar + className)
        val writer = new FileWriter(file)
        try{
          writer.write(treeToString(ast))
        }
        finally{
          writer.close()
        }
      }
    }

  }
}