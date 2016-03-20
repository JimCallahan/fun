/*
 * Copyright 2011-2016 James Michael Callahan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fun.util

import java.io.BufferedWriter
import java.io.FileWriter
import java.nio.file.Path

import scala.xml.Node
import scala.xml.PrettyPrinter
import scala.xml.Utility

/** Provides logged and pretty printed file writing of XML data. */
trait XMLSaver
    extends Logging {

  /** Convert to an XML representation without whitespace. */
  def trimmedXML: Node = Utility.trim(toXML)

  /** Convert to a pretty printed XML string. */
  def prettyXML: String = {
    val pp = new PrettyPrinter(100, 2)
    pp.format(toXML)
  }

  /** Write to an XML file. */
  def saveXML(path: Path): Unit = {
    log.info(s"Writing: $path")
    val out = new BufferedWriter(new FileWriter(path.toFile))
    try {
      out.write(prettyXML)
    }
    finally {
      out.close
    }
  }

  /** Convert to an XML representation. */
  def toXML: Node
}
