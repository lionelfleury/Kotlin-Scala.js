/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

// Modified by Guillaume Tournigand and Lionel Fleury

import java.io.{Console => _, _}

import org.scalajs.core.ir.Printers.IRTreePrinter
import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.tools.io._

import scala.collection.immutable.Seq

object Scalajsp {

  private case class Options(
                              infos: Boolean = false,
                              jar: Option[File] = None,
                              fileNames: Seq[String] = Seq.empty)

  def run(args: Array[String]): String = {

    val stdout = new StringWriter()
    val parser = new scopt.OptionParser[Options]("scalajsp") {
      head("scalajsp", ScalaJSVersions.current)
      arg[String]("<file> ...")
        .unbounded()
        .action { (x, c) => c.copy(fileNames = c.fileNames :+ x) }
        .text("*.sjsir file to display content of")

      override def showUsageOnError = true
    }

    for {
      options <- parser.parse(args, Options())
      fileName <- options.fileNames
    } {
      val vfile = readFromFile(fileName)
      new IRTreePrinter(stdout).printTopLevelTree(vfile.tree)
    }
    stdout.toString
  }

  private def fail(msg: String) = {
    Console.err.println(msg)
    sys.exit(1)
  }

  private def readFromFile(fileName: String) = {
    val file = new File(fileName)

    if (!file.exists)
      fail(s"No such file: $fileName")
    else if (!file.canRead)
      fail(s"Unable to read file: $fileName")
    else
      FileVirtualScalaJSIRFile(file)
  }

}