package com.wheaties

import treehugger.forest._
import java.io.{FileWriter, File}

package squealer{

  case class ParsedResult(classPackage: String, className: String, ast: Tree)

  //TODO: ok, time to move this to the generator area and probably substantially change it
  trait Recorder[T]{
    def record(result: ParsedResult):Unit
  }

  object FileRecorder extends Recorder[ParsedResult]{
    val basePath = "src" + File.pathSeparatorChar + "main" + File.pathSeparatorChar + "scala"

    def record(result: ParsedResult){
      val ParsedResult(classPackage, className, ast) = result

      val path = basePath + File.pathSeparatorChar + classPackage.replace('.', File.pathSeparatorChar)
      try{
        val filePath = new File(path)
        if(!filePath.exists()) filePath.mkdir()
        val file = new File(path + File.pathSeparatorChar + className + ".scala")
        write(file, ast)
      }
      catch{
        case ex => println(ex.getMessage)
      }
    }

    def write(file: File, ast: Tree)={
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