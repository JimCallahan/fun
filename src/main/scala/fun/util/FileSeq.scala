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

import java.nio.file.Path

/** A standard file naming convention for single or multiple frame sequences of the
  * form: prefix[.#][.suffix]
  * @param prefix The full path including file name prefix.
  * @param suffix An optional file name suffix.
  * @param range An optional frame range.
  * @param padding The amount of frame number padding (if any).
  */
class FileSeq private (
  val prefix: Path,
  val suffix: Option[String],
  val range: Option[Range],
  padding: Int
) {
  /** The sequence of paths specified. */
  def paths: Seq[Path] = {
    val sfx =
      suffix match {
        case Some(s) => "." + s
        case None    => ""
      }
    def f(frame: Int): Path = {
      val spec =
        padding match {
          case 0 => "%d"
          case n => "%0" + n + "d"
        }
      prefix.getParent.resolve(prefix.getFileName + "." + spec.format(frame) + sfx)
    }

    range match {
      case None    => List(prefix.getParent.resolve(prefix.getFileName + sfx))
      case Some(r) => r.map(f(_))
    }
  }
}

object FileSeq {
  /** Create a single file sequence with no suffix or frame numbers. */
  def apply(path: Path): FileSeq =
    new FileSeq(path, None, None, 0)

  /** Create a single file sequence with a suffix but no frame numbers. */
  def apply(prefix: Path, suffix: String): FileSeq =
    new FileSeq(prefix, Some(suffix), None, 0)

  /** Create a multiple file sequence with padded frame numbers but no suffix. */
  def apply(prefix: Path, range: Range): FileSeq =
    new FileSeq(prefix, None, Some(range), 4)

  /** Create a multiple file sequence with frame numbers but no suffix. */
  def apply(prefix: Path, range: Range, padding: Int): FileSeq =
    new FileSeq(prefix, None, Some(range), padding)

  /** Create a padded, multiple file sequence. */
  def apply(prefix: Path, suffix: String, range: Range): FileSeq =
    new FileSeq(prefix, Some(suffix), Some(range), 4)

  /** Create a multiple file sequence. */
  def apply(prefix: Path, suffix: String, range: Range, padding: Int): FileSeq =
    new FileSeq(prefix, Some(suffix), Some(range), padding)
}
