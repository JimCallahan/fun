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

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.file.Path
import java.nio.file.Paths

import javax.imageio.ImageIO

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.GL_RED
import org.lwjgl.opengl.GL11.GL_RGB
import org.lwjgl.opengl.GL11.GL_RGBA
import org.lwjgl.opengl.GL11.GL_TEXTURE_1D
import org.lwjgl.opengl.GL11.GL_TEXTURE_2D
import org.lwjgl.opengl.GL11.GL_UNSIGNED_BYTE
import org.lwjgl.opengl.GL11.glBindTexture
import org.lwjgl.opengl.GL11.glDeleteTextures
import org.lwjgl.opengl.GL11.glGenTextures
import org.lwjgl.opengl.GL11.glTexImage1D
import org.lwjgl.opengl.GL11.glTexImage2D
import org.lwjgl.opengl.GL11.glTexSubImage2D
import org.lwjgl.opengl.GL11.glTexParameteri
import org.lwjgl.opengl.GL12.GL_TEXTURE_BASE_LEVEL
import org.lwjgl.opengl.GL12.GL_TEXTURE_MAX_LEVEL
import org.lwjgl.opengl.GL12.glTexImage3D
import org.lwjgl.opengl.GL13.GL_TEXTURE0
import org.lwjgl.opengl.GL13.glActiveTexture
import org.lwjgl.opengl.GL30.GL_TEXTURE_2D_ARRAY
import org.lwjgl.opengl.GL30.glGenerateMipmap

import fun.math.Index2i
import fun.math.Index3i
import fun.math.Vec2d
import fun.math.Vec3d
import fun.util.ColorBrewer
import fun.util.ColorBrewer.ColorScheme
import fun.util.ColorBrewer.ColorName
import fun.util.FileSeq

import GLUtil.tron

/** General helper methods for textures. */
object TextureObject {
  /** Check that the given dimensions are a power of 2, returning that power. */
  def checkPowerOfTwo(size: Index2i): Vec2d = {
    import scala.math.log
    val power = size.toVec2d.map(c => log(c.toDouble) / log(2.0))
    if (power.forany(c => c.floor != c))
      throw new IllegalArgumentException(
        "The texture image resolution must be a power of (2)!")
    power
  }
}

/** The abstract base class of all OpenGL texture objects.
  * @constructor Create a new Texture Object.
  * @param target The target (OpenGL enum) to which kind of texture object is bound:
  * GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, or GL_TEXTURE_1D_ARRAY, GL_TEXTURE_2D_ARRAY,
  * GL_TEXTURE_RECTANGLE, GL_TEXTURE_CUBE_MAP, GL_TEXTURE_2D_MULTISAMPLE or
  * GL_TEXTURE_2D_MULTISAMPLE_ARRAY. */
abstract class TextureObject(val target: Int)
    extends BaseTextured("Texture") {

  /** Load texture data.
    * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
    * GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER,
    * GL_RGBA_INTEGER, GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT or GL_DEPTH_STENCIL.
    * @param dtype The type of data being supplied (OpenGL enum): GL_UNSIGNED_BYTE, GL_BYTE,
    * GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT, GL_UNSIGNED_BYTE_3_3_2,
    * GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5, GL_UNSIGNED_SHORT_5_6_5_REV,
    * GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV, GL_UNSIGNED_SHORT_5_5_5_1,
    * GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8, GL_UNSIGNED_INT_8_8_8_8_REV,
    * GL_UNSIGNED_INT_10_10_10_2 or GL_UNSIGNED_INT_2_10_10_10_REV.
    * @param data The pixel data to load. */
  def load(format: Int, dtype: Int, data: ByteBuffer): Unit

  /** Load texture data of GL_UNSIGNED_BYTE data type.
    * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
    * GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER,
    * GL_RGBA_INTEGER, GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT or GL_DEPTH_STENCIL.
    * @param data The pixel data to load.
    */
  def load(format: Int, data: ByteBuffer): Unit = load(format, GL_UNSIGNED_BYTE, data)

  /** Reload (overwritting the existing) texture data.
    * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
    * GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER, GL_RGBA_INTEGER,
    * GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT or GL_DEPTH_STENCIL.
    * @param dtype The type of data being supplied (OpenGL enum): GL_UNSIGNED_BYTE, GL_BYTE,
    * GL_UNSIGNED_SHORT, GL_SHORT, GL_UNSIGNED_INT, GL_INT, GL_FLOAT, GL_UNSIGNED_BYTE_3_3_2,
    * GL_UNSIGNED_BYTE_2_3_3_REV, GL_UNSIGNED_SHORT_5_6_5, GL_UNSIGNED_SHORT_5_6_5_REV,
    * GL_UNSIGNED_SHORT_4_4_4_4, GL_UNSIGNED_SHORT_4_4_4_4_REV, GL_UNSIGNED_SHORT_5_5_5_1,
    * GL_UNSIGNED_SHORT_1_5_5_5_REV, GL_UNSIGNED_INT_8_8_8_8, GL_UNSIGNED_INT_8_8_8_8_REV,
    * GL_UNSIGNED_INT_10_10_10_2 or GL_UNSIGNED_INT_2_10_10_10_REV.
    * @param data The pixel data to load. */
  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit

  /** Reload (overwritting the existing) texture data.
    * @param format The format (OpenGL enum) of the pixel data: GL_RED, GL_RG, GL_RGB, GL_BGR,
    * GL_RGBA, GL_BGRA, GL_RED_INTEGER, GL_RG_INTEGER, GL_RGB_INTEGER, GL_BGR_INTEGER, GL_RGBA_INTEGER,
    * GL_BGRA_INTEGER, GL_STENCIL_INDEX, GL_DEPTH_COMPONENT or GL_DEPTH_STENCIL.
    * @param data The pixel data to load. */
  def reload(format: Int, data: ByteBuffer): Unit = reload(format, GL_UNSIGNED_BYTE, data)

  /** The unique OpenGL handle for the texture object. */
  protected val objectID = {
    val id = tron { glGenTextures }
    state = Reserved
    id
  }

  protected def bindGL(textureUnit: Int): Unit = {
    tron { glActiveTexture(GL_TEXTURE0 + textureUnit) }
    tron { glBindTexture(target, objectID) }
  }

  protected def unbindGL(textureUnit: Int): Unit = {
    tron { glActiveTexture(GL_TEXTURE0 + textureUnit) }
    tron { glBindTexture(target, 0) }
  }

  protected def releaseGL(): Unit =
    tron { glDeleteTextures(objectID) }
}

object Texture1D {
  /** Create a 1-dimensional simple texture.
    * @param size The base mipmap level texture image size (should be power of 2).
    * @param iformat The internal data format (OpenGL enum).
    * See [[https://www.opengl.org/sdk/docs/man/html/glTexImage1D.xhtml Tables 1-3]] for
    * available formats. */
  def apply(size: Int, iformat: Int): Texture1D = new Texture1D(size, iformat)
}

/** A 1-dimensional simple texture. */
class Texture1D protected (val size: Int, val iformat: Int)
    extends TextureObject(GL_TEXTURE_1D) {
  def load(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexImage1D(target, 0, iformat, size, 0, format, dtype, data) }
    unbind(0)
  }

  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit = load(format, dtype, data)
}

object Texture2D {
  /** Create a 2-dimensional mipmapped texture.
    * @param size The base mipmap level texture image size (should be power of 2).
    * @param iformat The internal data format (OpenGL enum).
    * See [[https://www.opengl.org/sdk/docs/man/html/glTexImage2D.xhtml Tables 1-3]] for
    * available formats. */
  def apply(size: Index2i, iformat: Int): Texture2D = new Texture2D(size, iformat)

  /** Load a RGBA format 2D texture from file. */
  def apply(path: Path): Texture2D = {
    Option(getClass.getResourceAsStream(path.toString)) match {
      case None => throw new IOException("Unable to find texture image: " + path)
      case Some(istream) =>
        val image = ImageIO.read(istream)
        val raster = image.getRaster
        if (raster.getNumBands != 4) throw new IOException("The texture image is not RGBA: " + path)
        val b = raster.getBounds
        val size = Index2i(b.width, b.height)
        TextureObject.checkPowerOfTwo(size)

        val buf = BufferUtils.createByteBuffer(b.width * b.height * 4)

        for {
          y <- 0 until b.height
          x <- b.x until b.x + b.width
          s <- 0 until 4
        } buf.put(raster.getSample(x, b.y + b.height - y - 1, s).toByte)
        buf.rewind

        val tx = new Texture2D(size, GL_RGBA)
        tx.load(GL_RGBA, buf)
        buf.clear
        tx
    }
  }
}

/** A 2-dimensional mipmapped texture. */
class Texture2D protected (val size: Index2i, val iformat: Int)
    extends TextureObject(GL_TEXTURE_2D) {

  /** The number of mipmap levels. */
  val levels = (TextureObject.checkPowerOfTwo(size).reduce(scala.math.min)).toInt

  def load(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0) }
    tron { glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, levels) }
    tron { glTexImage2D(target, 0, iformat, size.x, size.y, 0, format, dtype, data) }
    tron { glGenerateMipmap(target) }
    unbind(0)
  }

  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    throw new NotImplementedError
  }
}

object VideoTexture {
  /** Create a 2-dimensional rectangular texture frequently updated with video frames.
    * @param size The texture image size.
    * @param iformat The internal data format (OpenGL enum).
    * See [[https://www.opengl.org/sdk/docs/man/html/glTexImage2D.xhtml Tables 1-3]] for
    * available formats. */
  def apply(size: Index2i, iformat: Int): VideoTexture =
    new VideoTexture(size, iformat)
}

/** A 2-dimensional rectangular texture frequently updated with video frames. */
class VideoTexture private (val size: Index2i, val iformat: Int)
    extends TextureObject(GL_TEXTURE_2D) {

  def load(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0) }
    tron { glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, 0) }
    tron { glTexImage2D(target, 0, iformat, size.x, size.y, 0, format, dtype, data) }
    unbind(0)
  }

  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexImage2D(target, 0, iformat, size.x, size.y, 0, format, dtype, data) }
    unbind(0)
  }
}

object VectorTexture2D {
  /** Create a 2-dimensional height field texture representing vector art at the 0.5 level-set
    * from an 8-bit grey-scale image.
    * @param path The path to the texture image. */
  def apply(path: Path): VectorTexture2D = {
    Option(getClass.getResourceAsStream(path.toString)) match {
      case None => throw new IOException("Unable to find texture image: " + path)
      case Some(istream) =>
        val image = ImageIO.read(istream)
        val raster = image.getRaster
        val b = raster.getBounds
        val size = Index2i(b.width, b.height)
        TextureObject.checkPowerOfTwo(size)

        val buf = BufferUtils.createByteBuffer(b.width * b.height)

        for {
          y <- 0 until b.height
          x <- b.x until b.x + b.width
        } buf.put(raster.getSample(x, b.y + b.height - y - 1, 0).toByte)
        buf.rewind

        val vt = new VectorTexture2D(size)
        vt.load(buf)
        buf.clear
        vt
    }
  }
}

/** A 2-dimensional height field texture representing vector art at the 0.5 level-set. */
class VectorTexture2D private (val size: Index2i)
    extends TextureObject(GL_TEXTURE_2D) {

  def load(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    throw new NotImplementedError("Use load(ByteBuffer) instead.")
  }

  /** Load texture data of GL_RED format and GL_UNSIGNED_BYTE data type.
    * @param data The pixel data to load.
    */
  def load(data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0) }
    tron { glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, 0) }
    tron { glTexImage2D(target, 0, GL_RED, size.x, size.y, 0, GL_RED, GL_UNSIGNED_BYTE, data) }
    unbind(0)
  }

  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    throw new NotImplementedError("Use reload(ByteBuffer) instead.")
  }

  /** Reload (overwritting the existing) texture data of GL_RED format and GL_UNSIGNED_BYTE data type.
    * @param data The pixel data to load. */
  def reload(data: ByteBuffer): Unit = load(data)
}

object VectorTexture2DArray {
  /** Create an array of 2-dimensional height field textures representing vector art at the 0.5
    * level-set from an 8-bit grey-scale image.
    * @param fseq The paths of texture images. */
  def apply(fseq: FileSeq): VectorTexture2DArray = {
    val rs = fseq.paths.map {
      case path =>
        val istream = getClass.getResourceAsStream(path.toString)
        val image = ImageIO.read(istream)
        val raster = image.getRaster
        val b = raster.getBounds
        val size = Index2i(b.width, b.height)
        TextureObject.checkPowerOfTwo(size)
        (raster, size)
    } take (256)

    val isize =
      rs.splitAt(1) match {
        case (Seq((_, sz)), Seq()) => sz
        case (Seq((_, sz)), tl) =>
          if (tl.exists(_._2 != sz))
            throw new IOException(s"The texture images did not all have (${sz.x}x${sz.y}) resolution!")
          sz
      }
    val size = isize.toIndex3i.updateZ(rs.size)
    val buf = BufferUtils.createByteBuffer(size.reduce(_ * _))

    for {
      (raster, _) <- rs
      b = raster.getBounds
      y <- 0 until b.height
      x <- b.x until b.x + b.width
    } buf.put(raster.getSample(x, b.y + b.height - y - 1, 0).toByte)
    buf.rewind

    val vt = new VectorTexture2DArray(size)
    vt.load(buf)
    buf.clear
    vt
  }
}

/** An array of 2-dimensional height field textures representing vector art at the 0.5 level-set. */
class VectorTexture2DArray private (val size: Index3i)
    extends TextureObject(GL_TEXTURE_2D_ARRAY) {

  def load(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    throw new NotImplementedError("Use load(ByteBuffer) instead.")
  }

  /** Load an array of texture data of GL_RED format and GL_UNSIGNED_BYTE data type.
    * @param data The pixel data to load. */
  def load(data: ByteBuffer): Unit = {
    bind(0)
    tron { glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0) }
    tron { glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, 0) }
    tron { glTexImage3D(target, 0, GL_RED, size.x, size.y, size.z, 0, GL_RED, GL_UNSIGNED_BYTE, data) }
    unbind(0)
  }

  def reload(format: Int, dtype: Int, data: ByteBuffer): Unit = {
    throw new NotImplementedError("Use reload(ByteBuffer) instead.")
  }

  /** Reload (overwritting the existing) an array of texture data of GL_RED format and
    * GL_UNSIGNED_BYTE data type.
    * @param data The pixel data to load. */
  def reload(data: ByteBuffer): Unit = load(data)
}

object TestTexture2D {
  /** Create a procedurally generated 2-dimensional mipmapped texture suitable for testing.
    * @param size The size of the square texture in both dimensions. */
  def apply(size: Int): Texture2D = {
    val tex = new TestTexture2D(size)
    val buf = BufferUtils.createByteBuffer(size * size * 4)
    val eigth = size / 8
    for {
      y <- 0 until size
      x <- 0 until size
    } {
      val v = Index2i(x, y).toVec2d / Index2i(size).toVec2d
      val rg = (v * 255.0).toIndex2i
      if (((x / eigth) + (y / eigth)) % 2 == 0) {
        buf.put(rg.x.toByte)
        buf.put(rg.y.toByte)
      }
      else {
        buf.put(128.toByte)
        buf.put(128.toByte)
      }

      buf.put(((v.x * v.y) * 255.0).toByte)
      buf.put(255.toByte)
    }
    buf.rewind
    tex.load(GL_RGBA, buf)
    tex
  }
}

/** A procedurally generated 2-dimensional mipmapped texture suitable for testing. */
class TestTexture2D private (val sz: Int)
    extends Texture2D(Index2i(sz), GL_RGBA)

object PaletteTexture {
  /** Generate a 1-dimensional OpenGL texture object of GL_RGB format and GL_UNSIGNED_BYTE data type
    * based on interpolating the given ColorBrewer palette.
    * @param size The size of the texture. */
  def apply(palette: IndexedSeq[Vec3d], size: Int): PaletteTexture = {
    val tex = new PaletteTexture(size)
    val buf = BufferUtils.createByteBuffer(size * 3)
    for (i <- 0 until size) {
      val rgb = (ColorBrewer.pickColor(palette, i.toDouble / size.toDouble) * 255.0).toIndex3i
      buf.put(rgb.x.toByte)
      buf.put(rgb.y.toByte)
      buf.put(rgb.z.toByte)
    }
    buf.rewind
    tex.load(GL_RGB, buf)
    tex
  }

  /** Generate a 1-dimensional OpenGL texture object of GL_RGB format and GL_UNSIGNED_BYTE data type
    * based on interpolating the given ColorBrewer palette.
    * @param size The size of the texture. */
  def apply(scheme: ColorScheme, cname: ColorName, numColors: Int, size: Int): PaletteTexture = {
    val p = ColorBrewer.palette(scheme)(cname)(numColors)
    PaletteTexture(p, size)
  }
}

/** A 1-dimensional OpenGL texture object based on interpolating a ColorBrewer palette. */
class PaletteTexture private (sz: Int)
    extends Texture1D(sz, GL_RGB)
