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
package fun.opengl

import java.awt.Font
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.nio.file.Path
import java.nio.file.Paths

import javax.imageio.ImageIO

import fun.math.BBox2d
import fun.math.Pos2d
import fun.util.FileSeq
import fun.util.Logging
import fun.util.XMLSaver

import scala.collection.immutable.TreeMap
import scala.xml.Node
import scala.xml.XML

/** Metric information about a specific textured glyph.
  * @constructor Create new metric information for a textured glyph.
  * @param index The index (depth) of the glyph within the 3D texture.
  * @param char The character represented by the glyph.
  * @param advance The amount to advance the position (in texture coords) after rendering the glyph.
  * @param bbox The bounds (relative to the glyph anchor in texture coords) of the visible glyph.
  */
case class FontGlyph(index: Int, char: Char, advance: Double, bbox: BBox2d)
    extends Ordered[FontGlyph] {

  /** Ordered by increasing index. */
  def compare(that: FontGlyph): Int = index compare that.index

  /** Convert to tab separated text format. */
  def toXML: Node = {
    <Glyph>
    <Index>{ index }</Index>
    <Char>{ char.toInt }</Char>
    <Advance>{ advance }</Advance>
    <Bounds>{ bbox.bmin.x + " " + bbox.bmin.y + " " + bbox.bmax.x + " " + bbox.bmax.y }</Bounds>
    </Glyph>
  }

  /** Convert to a string representation. */
  override def toString =
    s"Glyph($index, ${char.toInt}, $advance)"
}

object FontGlyph {
  /** Create a glyph from XML data. */
  def fromXML(node: Node): FontGlyph = {
    val index = (node \ "Index").text.toInt
    val char = (node \ "Char").text.toInt.toChar
    val advance = (node \ "Advance").text.toDouble
    val bbox =
      (node \ "Bounds").text.split("\\p{Space}").filter(!_.isEmpty).map(_.toDouble) match {
        case Array(nx, ny, mx, my) => BBox2d(Pos2d(nx, ny), Pos2d(mx, my))
        case _                     => throw new IOException("Illegal bounding box!")
      }
    FontGlyph(index, char, advance, bbox)
  }
}

/** A set of textured glyphs corresponding to a specific TrueType font.
  *
  * @constructor Create a new textured font.
  * @param name The name of the TrueType font.
  * @param glyphs The metrics used to render the set of textured glyphs.
  * @param anchor The texture coordinate of the anchor (baseline) of the glyph.
  */
class VectorFontTextures private (val name: String, val glyphs: Map[Char, FontGlyph], val anchor: Pos2d)
    extends XMLSaver {

  /** The maximum vertical distance from the baseline to the top of all glyphs for the
    * font (in texture coordinates).
    */
  lazy val ascent: Double =
    (0.0 /: glyphs.toSeq) {
      case (mx, (_, g)) => mx max g.bbox.bmax.y
    }

  /** The maximum vertical distance from the baseline to the bottom of all glyphs for the
    * font (in texture coordinates).
    */
  lazy val descent: Double =
    (0.0 /: glyphs.toSeq) {
      case (mx, (_, g)) => mx max -g.bbox.bmin.y
    }

  /** The maximum vertical distance from the top of all glyphs to the bottom of all glyphs for the
    * font (in texture coordinates).
    */
  lazy val extent: Double = ascent + descent

  /** The average vertical distance from the baseline to the top of most lower-case characters
    * (in texture coordinates). */
  lazy val height: Double = {
    val cs = Seq('a', 'e', 'm', 'n', 'o', 'r', 's', 'u', 'v', 'w', 'x', 'z')
    val vs = cs.flatMap(glyphs.get(_)).map(_.bbox.bmax.y)
    vs.reduce(_ + _) / vs.size.toDouble
  }

  /** Load the font textures into an OpenGL 3D texture map.
    * @param dir The directory containing the glyph textures.
    */
  def loadTextures(dir: Path): VectorTexture2DArray = {
    val fseq = FileSeq(dir.resolve("glyph"), "png", 0 until glyphs.size, 4)
    VectorTexture2DArray(fseq)
  }

  /** Convert to tab separated text format. */
  def toXML: Node = {
    (<VectorFontTexture>
      <Name>{ name }</Name>
      <Anchor>{ anchor.x + " " + anchor.y }</Anchor>
      <Glyphs>{
        glyphs.map {
          case (_, g)=> g.toXML
        }
      }</Glyphs>
      </VectorFontTexture>)
  }
}

/** A factory for TexFont. */
object VectorFontTextures
    extends Logging {

  /** Render and height field encode the individual glyphs from a TrueType specification file.
    * @param fontSpec The location of the TrueType font specification file.
    * @param outDir Directory to write the generated textures and metrics XML file.
    * @param res The square image resolution (must be power of two).
    */
  def render(fontSpec: Path, outDir: Path, res: Int): VectorFontTextures = {
    import scala.math

    if (res != math.pow(2.0, math.ceil(math.log(res) / math.log(2.0))).toInt)
      throw new IllegalArgumentException("The image resolution must be a power of two!")

    val scale = 0.6
    val anchor = Pos2d(0.2, 0.3)
    val pointSize = res.toDouble * scale

    // Load the TrueType font and size it appropriately.
    val font = Font.createFont(Font.TRUETYPE_FONT, fontSpec.toFile)
    val fontSized = font.deriveFont(pointSize.toFloat)

    // Determine which characters to render.
    val legal: Array[Char] = {
      val cs =
        for {
          i <- 33 to 126 // Printable ASCII Range
          c = i.toChar
          if (fontSized.canDisplay(c))
            } yield c
      cs.toArray
    }

    // Compute number of texture maps required.
    val size = legal.size

    // Output names and paths.
    val prefix = outDir.resolve("glyph")
    val rawPrefix = outDir.resolve(Paths.get("raw", "char"))

    // Figure out the nearest power of two texture map resolution.
    val bbox = BBox2d(Pos2d(0.0), Pos2d(res))
    val pos = bbox.position(Pos2d(anchor.x, 1.0 - anchor.y))

    // Render all visible glyphs.
    val raw =
      legal.flatMap {
        c =>
        // Create an image to render into.
        val img = new BufferedImage(res, res, BufferedImage.TYPE_BYTE_GRAY)

        // Set up the graphics context for anti-aliased text.
        val gfx = img.createGraphics
        gfx.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        // Render the glyph.
        val gvec = fontSized.createGlyphVector(gfx.getFontRenderContext, Array(c))
        gfx.drawGlyphVector(gvec, pos.x.toFloat, pos.y.toFloat)

        // Check to see if anything was rendered to texture.
        val isBlack = {
          val xy = for {
            x <- 0 until res
            y <- 0 until res
          } yield (x, y)
          val raster = img.getRaster
          xy.forall {
            case (x, y) => raster.getSample(x, y, 0) == 0
          }
        }

        val metrics = gvec.getGlyphMetrics(0)

        // The amount to advance the cursor after rendering (in texture coords).
        val advance = metrics.getAdvance.toDouble / pointSize

        // The bounding box in texture coords.
        val bbox = {
          val r = metrics.getBounds2D
          BBox2d(
            Pos2d(r.getX, -(r.getY + r.getHeight)),
            Pos2d(r.getX + r.getWidth, -r.getY)).map(_ / res.toDouble
          )
        }

        if (isBlack) None
        else {
          // Save the raw image to disk.
          val path = FileSeq(rawPrefix, "png", c.toInt to c.toInt).paths(0)
          log.info(s"Writing: $path")
          path.getParent.toFile.mkdirs
          ImageIO.write(img, "png", path.toFile)

          Some((c, advance, path, bbox))
        }
      }

    // Encode the textures and assemble the glyph metrics table.
    val glyphs = {
      val texPaths = FileSeq(prefix, "png", 0 until raw.size, 4).paths
      val gs = texPaths.zipWithIndex.toArray.par.map {
        case (path, i) =>
          val (c, adv, rpath, bb) = raw(i)

          log.info(s"Encoding [${rpath.getFileName}]: $path")
          path.getParent.toFile.mkdirs
          VectorTexture.encode(rpath, path, res / 8, 0.15)
          rpath.toFile.delete

          FontGlyph(i, c, adv, bb)
      }

      rawPrefix.getParent.toFile.delete

      (TreeMap[Char, FontGlyph]() /: gs) {
        case (m, g) => m.updated(g.char, g)
      }
    }

    val metrics = new VectorFontTextures(font.getName, glyphs, anchor)
    metrics.saveXML(outDir.resolve("metrics.xml"))
    metrics
  }

  /** Read the font metrics from XML file.
    * @param dir The directory containing font metrics file.
    */
  def load(dir: Path): VectorFontTextures = {
    val path = dir.resolve("metrics.xml")
    log.info(s"Reading: $path")
    val istream = getClass.getResourceAsStream(path.toString)
    val in = new BufferedReader(new InputStreamReader(istream))
    try {
      fromXML(XML.load(in))
    }
    finally {
      in.close
    }
  }

  /** Create font metrics from XML data. */
  def fromXML(node: Node): VectorFontTextures = {
    val name = (node \ "Name").text
    val anchor = Pos2d((node \ "Anchor").text.split("\\p{Space}").map(_.toDouble))
    val fgs = for (g <- (node \ "Glyphs" \ "Glyph")) yield FontGlyph.fromXML(g)
    val glyphs = (TreeMap[Char, FontGlyph]() /: fgs) {
      case (m, g) => m.updated(g.char, g)
    }
    new VectorFontTextures(name, glyphs, anchor)
  }
}
