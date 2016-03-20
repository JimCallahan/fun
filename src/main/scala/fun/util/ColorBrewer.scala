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

import java.io.IOException
import java.nio.file.Paths

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import fun.math.BBox1d
import fun.math.Index3i
import fun.math.Scalar
import fun.math.Space1d
import fun.math.Vec3d
import fun.opengl.Texture1D

/** The color schemes of Cynthia Brewer.
  * @see Cynthia A. Brewer, 1994, ''"Color Use Guidelines for Mapping and Visualization,"''
  * Chapter 7 (pp. 123-147) in Visualization in Modern Cartography, edited by A.M. MacEachren
  * and D.R.F. Taylor, Elsevier Science, Tarrytown, NY.
  * @see Cynthia A. Brewer, 1994, ''"Guidelines for Use of the Perceptual Dimensions of Color
  * for Mapping and Visualization,"'' Color Hard Copy and Graphic Arts III, edited by J. Bares,
  * Proceedings of the International Society for Optical Engineering (SPIE), San Jose, February
  * 1994, Vol. 2171, pp. 54-63.
  */
object ColorBrewer
    extends Logging {

  /** The scheme for mapping values to colors. */
  sealed trait ColorScheme

  /** Sequential schemes are suited to ordered data that progress from low to high. Lightness
    * steps dominate the look of these schemes, with light colors for low data values to dark
    * colors for high data values. */
  case object Sequential extends ColorScheme

  /** Diverging schemes put equal emphasis on mid-range critical values and extremes at both
    * ends of the data range. The critical class or break in the middle of the legend is
    * emphasized with light colors and low and high extremes are emphasized with dark colors
    * that have contrasting hues. */
  case object Diverging extends ColorScheme

  /** Qualitative schemes do not imply magnitude differences between legend classes, and hues
    * are used to create the primary visual differences between classes. Qualitative schemes
    * are best suited to representing nominal or categorical data. */
  case object Qualitative extends ColorScheme

  /** Named color palettes for use with ColorBrewer.
    * @see The [[http://colorbrewer2.org original site]] for interactive display of the colors. */
  sealed trait ColorName

  case object Accent extends ColorName
  case object Blues extends ColorName
  case object BrBG extends ColorName
  case object BuGn extends ColorName
  case object BuPu extends ColorName
  case object Dark2 extends ColorName
  case object GnBu extends ColorName
  case object Greens extends ColorName
  case object Greys extends ColorName
  case object Oranges extends ColorName
  case object OrRd extends ColorName
  case object Paired extends ColorName
  case object Pastel1 extends ColorName
  case object Pastel2 extends ColorName
  case object PiYG extends ColorName
  case object PRGn extends ColorName
  case object PuBu extends ColorName
  case object PuBuGn extends ColorName
  case object PuOr extends ColorName
  case object PuRd extends ColorName
  case object Purples extends ColorName
  case object RdBu extends ColorName
  case object RdGy extends ColorName
  case object RdPu extends ColorName
  case object RdYlBu extends ColorName
  case object RdYlGn extends ColorName
  case object Reds extends ColorName
  case object Set1 extends ColorName
  case object Set2 extends ColorName
  case object Set3 extends ColorName
  case object Spectral extends ColorName
  case object YlGn extends ColorName
  case object YlGnBu extends ColorName
  case object YlOrBr extends ColorName
  case object YlOrRd extends ColorName

  private lazy val colors: Map[ColorScheme, Map[ColorName, Map[Int, IndexedSeq[Vec3d]]]] = {
    val path = Paths.get("/color/color-brewer.csv")
    Option(getClass.getResourceAsStream(path.toString)) match {
      case Some(rs) =>
        val source = Source.fromInputStream(rs)
        log.info(s"Reading: $path")
        Try {
          var cs: List[(ColorScheme, ColorName, Int, Array[Vec3d])] = Nil
          var colors = Array(Vec3d(0.0))
          def decode(i: String, r: String, g: String, b: String) {
            colors(i.toInt - 1) = Index3i(r.toInt, g.toInt, b.toInt).toVec3d / Vec3d(255.0)
          }
          val numColors = 9
          for (line <- source.getLines.drop(1)) {
            val cols = line.split(",").take(numColors)
            cols match {
              case Array("", "", "", _, i, _, r, g, b) =>
                decode(i, r, g, b)
              case Array(cn, nc, tp, _, i, _, r, g, b) =>
                val colorName = {
                  val cns = Seq(
                    Accent, Blues, BrBG, BuGn, BuPu, Dark2,
                    GnBu, Greens, Greys, Oranges, OrRd,
                    Paired, Pastel1, Pastel2,
                    PiYG, PRGn, PuBu, PuBuGn, PuOr, PuRd, Purples,
                    RdBu, RdGy, RdPu, RdYlBu, RdYlGn, Reds,
                    Set1, Set2, Set3, Spectral,
                    YlGn, YlGnBu, YlOrBr, YlOrRd
                  )
                  cns.find(_.toString == cn)
                    .getOrElse(throw new IOException(s"Unknown ColorName: $cn"))
                }
                val numColors = nc.toInt
                val colorScheme = tp match {
                  case "seq"  => Sequential
                  case "div"  => Diverging
                  case "qual" => Qualitative
                  case _      => throw new IOException("Illegal Color Scheme: " + tp)
                }
                colors = Array.fill(numColors)(Vec3d(0.0))
                cs = (colorScheme, colorName, numColors, colors) :: cs
                decode(i, r, g, b)
              case _ => throw new IOException("Bad color specification: " + line)
            }
          }
          val ics = cs.map {
            case (s, n, k, c) => (s, n, k, c.toIndexedSeq)
          }
          ImmutableMaps.mapMapMap(ics)
        } match {
          case Success(cm) => cm
          case Failure(ex) =>
            log.error(s"Unable to load: $path\n$ex")
            Map.empty
        }

      case _ =>
        log.error(s"File Missing: $path")
        Map()
    }
  }

  /** Choose a color palette based on scheme, name and number of colors.
    * @note Not all palette names are supported by all schemes.
    * @see The [[http://colorbrewer2.org original site]] for interactive display of the colors.
    */
  def palette(scheme: ColorScheme)(cname: ColorName)(numColors: Int): IndexedSeq[Vec3d] =
    colors(scheme)(cname)(numColors)

  /** Choose an interpolated color from the given palette.
    * @param palette The colors.
    * @param alpha A value [0, 1] used to interpolated the range of colors in the palette.
    */
  def pickColor(palette: IndexedSeq[Vec3d], alpha: Double): Vec3d = {
    import Scalar.{ clamp, doubleToPos1d, intToIndex1i, index1iToInt, vec1dToDouble }
    val s = Space1d(palette.size, BBox1d(0.0, 1.0))
    val mx = palette.size - 1
    val (i, o, p) = s.interpOf(alpha)
    val (ia, ib) = (i.clamp(0, mx), (i + o).clamp(0, mx))
    palette(ia).lerp(palette(ib), p)
  }
}
