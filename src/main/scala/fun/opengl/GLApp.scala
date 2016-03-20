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
import java.nio.file.Path

import javax.imageio.ImageIO

import org.lwjgl.BufferUtils
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.OpenGLException

import scala.annotation.tailrec

import fun.math.Index2i
import fun.math.Vec2d
import fun.util.Logging

abstract class GLApp
    extends App
    with GLAppLike
    with Logging {

  val stencilBits: Int = 8

  def displaySize: Vec2d = Vec2d(Display.getWidth, Display.getHeight)

  /** Paths to the 128x128, 32x32 and 16x16 images to use for the application icons. */
  def iconPaths: Seq[Path] = Nil

  def loadIcons(): Unit =
    if (iconPaths.nonEmpty) {
      val icons =
        iconPaths.flatMap {
          case path =>
            try {
              log.info(s"Reading: $path")
              val istream = getClass.getResourceAsStream(path.toString)
              val image = ImageIO.read(istream)
              val raster = image.getRaster
              val b = raster.getBounds
              val size = Index2i(b.width, b.height)
              TextureObject.checkPowerOfTwo(size)
              if (raster.getNumBands != 4)
                throw new IOException("Icon image was not RGBA!")
              Some((raster, size))
            }
            catch {
              case ex: Exception =>
                log.warn(s"Unable to load icon image, ignoring: $path\n  " + ex)
                None
            }
        }

      if (icons.nonEmpty) {
        val bufs = icons.toArray.map {
          case (raster, res) =>
            val size = res.reduce(_ * _) * 4
            val buf = BufferUtils.createByteBuffer(size)
            val b = raster.getBounds
            for {
              y <- 0 until b.height
              x <- b.x until b.x + b.width
              s <- 0 until 4
            } buf.put(raster.getSample(x, b.y + y, s).toByte)
            buf.rewind
            buf
        }
        Display.setIcon(bufs)
      }
    }

  /** The top-level. */
  try {
    log.info(s"Welcome to $appName!  (Esc to quit)")
    GLUtil.initNative

    log.info("CommandLine: " + args.mkString(" "))

    val fullscreen: Boolean = {
      Option(args) match {
        case Some(as) => as.contains("--fullscreen")
        case _        => false
      }
    }

    val initialSize: Option[Index2i] = {
      Option(args) match {
        case Some(as) => as.find(_.startsWith("--display=")) match {
          case Some(arg) =>
            arg.drop(10).split("x") match {
              case Array(xstr, ystr) =>
                try {
                  Some(Index2i(xstr.toInt, ystr.toInt))
                }
                catch {
                  case ex: NumberFormatException =>
                    log.warn("Invalid display specification: " + arg)
                    None
                }
            }
          case _ => None
        }
        case _ => None
      }
    }

    GLUtil.createDisplay(fullscreen, initialSize, stencilBits)
    Display.setTitle(appName)
    GLUtil.printInfo

    loadIcons

    @tailrec
    def loop(state: GraphicsState): Unit = {
      GLUtil.tronFrame()
      Display.update
      Display.sync(60)

      val esc = enableEscToExit && Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)
      if (!state.isAlive || esc || Display.isCloseRequested()) {
        state.onExit()
      }
      else {
        loop(state.render)
      }
    }

    loop(initial)

    Display.destroy

    log.info("Done.")
    sys.exit
  }
  catch {
    case ex: OpenGLException => report(ex, "GL ERROR")
    case ex: Exception       => report(ex)
  }
}
