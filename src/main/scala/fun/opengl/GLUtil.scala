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

import java.io.FileOutputStream
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path

import org.lwjgl.opengl.ContextAttribs
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.DisplayMode
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL11.GL_BLEND
import org.lwjgl.opengl.GL11.GL_COLOR_BUFFER_BIT
import org.lwjgl.opengl.GL11.GL_DEPTH_BUFFER_BIT
import org.lwjgl.opengl.GL11.GL_EXTENSIONS
import org.lwjgl.opengl.GL11.GL_ONE_MINUS_SRC_ALPHA
import org.lwjgl.opengl.GL11.GL_SRC_ALPHA
import org.lwjgl.opengl.GL11.GL_VERSION
import org.lwjgl.opengl.GL11.glBlendFunc
import org.lwjgl.opengl.GL11.glClear
import org.lwjgl.opengl.GL11.glClearColor
import org.lwjgl.opengl.GL11.glEnable
import org.lwjgl.opengl.GL11.glGetString
import org.lwjgl.opengl.GL11.glPolygonMode
import org.lwjgl.opengl.OpenGLException
import org.lwjgl.opengl.PixelFormat
import org.lwjgl.opengl.Util

import fun.math.Index2i
import fun.math.Vec2d
import fun.math.Vec3d
import fun.util.Logging

object GLUtil
    extends Logging {

  /** Kinds of operating systems. */
  sealed trait OsType
  case object Linux extends OsType
  case object MacOSX extends OsType
  case object Windows extends OsType

  /** The native memory model. */
  sealed trait OsModel
  case object Bit32 extends OsModel
  case object Bit64 extends OsModel
  case object BitUnknown extends OsModel

  /** The operating system running the application. */
  val os: OsType = {
    sys.props("os.name") match {
      case "Linux"    => Linux
      case "Mac OS X" => MacOSX
      case "Windows XP" | "Windows 2000" | "Windows 2003" | "Windows NT" | "Windows Vista" |
          "Windows 7" | "Windows 8.1" => Windows
      case n => throw new Error("Operating system (" + n + ") is not supported!")
    }
  }

  /** Unpack the native libraries for LWJGL from the parent JAR into a temporary directory
    * and initialize hidden LWJGL system property for where to load native libraries to this
    * location. */
  def initNative(): Unit = {
    val path = Files.createTempDirectory("lwgjl-natives-")
    val natives: Seq[Path] =
      os match {
        case Linux =>
          Seq("libjinput-linux.so", "libjinput-linux64.so", "liblwjgl.so", "liblwjgl64.so",
            "libopenal.so", "libopenal64.so").map {
            case p => Paths.get("linux", p)
          }
        case MacOSX =>
          Seq("libjinput-osx.dylib", "libjinput-osx64.dylib", "liblwjgl.dylib",
            "liblwjgl64.dylib", "openal.dylib", "openal64.dylib").map {
            case p => Paths.get("macosx", p)
          }
        case Windows =>
          Seq("jinput-dx8.dll", "jinput-dx8_64.dll", "jinput-raw.dll", "jinput-raw_64.dll", "lwjgl.dll",
            "lwjgl64.dll", "OpenAL32.dll", "OpenAL64.dll").map {
            case p => Paths.get("windows", p)
          }
      }

    natives foreach {
      case native =>
        try {
          val rpath = Paths.get("/").resolve(native)
          log.info(s"Copying Resource: $rpath")
          Option(getClass.getResourceAsStream(rpath.toString)) match {
            case Some(in) =>
              try {
                val tpath = path.resolve(native.getFileName)
                val file = tpath.toFile
                val out = new FileOutputStream(file)
                try {
                  val buf = Array.fill(2048)(0.toByte)
                  var done = false
                  while(!done) {
                    in.read(buf, 0, buf.size) match {
                      case -1 => done = true
                      case n  => out.write(buf, 0, n)
                    }
                  }
                }
                finally {
                  out.close()
                  file.deleteOnExit()
                }
              }
              finally {
                in.close()
              }
            case None =>
              log.error(s"Native LWJGL library missing from JAR: $native")
          }
        }
        catch {
          case ex: Exception =>
            log.error(s"Failed to extract LWJGL native library: $native")
        }
    }

    log.info("LWJGL Native Library Path: " + path)
    System.setProperty("org.lwjgl.librarypath", path.toFile.getAbsolutePath)
  }

  /** The frequency of calls to glGetError to check/report OpenGL error state. */
  sealed trait GLDebugLevel
  object GLDebugLevel {
    val levels = Seq(GLDebugOff, GLDebugInitial, GLDebugPerFrame, GLDebugPerCall)
  }

  /** Never check. */
  case object GLDebugOff extends GLDebugLevel

  /** Only check after initializing the Display and OpenGL context. */
  case object GLDebugInitial extends GLDebugLevel

  /** Check once per render frame. */
  case object GLDebugPerFrame extends GLDebugLevel

  /** Check after every significant call. */
  case object GLDebugPerCall extends GLDebugLevel

  /** The current global OpenGL debugging level. */
  private var debugLevel: GLDebugLevel =
    Option(System.getProperty("checkGL")) match {
      case Some("Off")      => GLDebugOff
      case Some("Initial")  => GLDebugInitial
      case Some("PerFrame") => GLDebugPerFrame
      case Some("PerCall")  => GLDebugPerCall
      case Some(v) =>
        log.warn("""Illegal "checkGL" property given, must be one of: """ +
          GLDebugLevel.levels.mkString(", "))
        GLDebugInitial
      case None => GLDebugInitial
    }
  log.info(s"OpenGL Error Checking: $debugLevel")

  /** Set the current OpenGL debugging level. */
  def setDebugLevel(level: GLDebugLevel): Unit = {
    log.info(s"OpenGL Error Checking: $level")
    debugLevel = level
  }

  /** Whether to abort the program on first OpenGL error. */
  private val abortOnError: Boolean = {
    val abort =
      try {
        val on = Seq("on", "true")
        val off = Seq("off", "false")
        Option(System.getProperty("abortGL")) match {
          case Some(v) if (on.contains(v.toLowerCase))  => true
          case Some(v) if (off.contains(v.toLowerCase)) => false
          case None                                     => false
          case _ =>
            log.warn("""Illegal "abortGL" property given, must be one of: """ +
              (on ++ off).map("\"" + _ + "\"").mkString(", "))
            false
        }
      }
      catch {
        case ex: Exception =>
          log.warn("""Unable to determine "abortGL" setting!""")
          false
      }
    log.info("Abort on OpenGL Errors: " + (if (abort) "On" else "Off"))
    abort
  }

  /** Whether to report every OpenGL error check (successful or not). */
  private val traceChecks: Boolean = {
    val abort =
      try {
        val on = Seq("on", "true")
        val off = Seq("off", "false")
        Option(System.getProperty("traceGL")) match {
          case Some(v) if (on.contains(v.toLowerCase))  => true
          case Some(v) if (off.contains(v.toLowerCase)) => false
          case None                                     => false
          case _ =>
            log.warn("""Illegal "traceGL" property given, must be one of: """ +
              (on ++ off).map("\"" + _ + "\"").mkString(", "))
            false
        }
      }
      catch {
        case ex: Exception =>
          log.warn("""Unable to determine "traceGL" setting!""")
          false
      }
    log.info("Trace on OpenGL Error Checks: " + (if (abort) "On" else "Off"))
    abort
  }

  /** Check/report OpenGL error state for the enclosed OpenGL calls. */
  def tron[A](body: => A): A = tronHelper(GLDebugPerCall, true, true, body)

  /** Check/report OpenGL error state after initialization of the context. */
  def tronInitial(): Unit = tronHelper(GLDebugInitial, false, true, {})

  /** Check/report OpenGL error state at the start of a rendering frame. */
  def tronFrame(): Unit = tronHelper(GLDebugPerFrame, true, false, {})

  /** Perform the appropriate amount of checking/reporting of OpenGL error state for the
    * enclosed OpenGL calls.
    * @param level
    * @param checkBefore
    * @param checkAfter
    * @param body
    */
  def tronHelper[A](
    level: GLDebugLevel,
    checkBefore: Boolean,
    checkAfter: Boolean,
    body: => A
  ): A = {
    def tronCheck(title: String): Unit = {
      var found = false
        (debugLevel, level) match {
        case (_, GLDebugOff) =>
          throw new IllegalArgumentException(
            "The (Off) OpenGL debug level only makes sense as a global setting!")
        case (GLDebugInitial, GLDebugInitial) |
            (GLDebugPerFrame, GLDebugInitial | GLDebugPerFrame) |
            (GLDebugPerCall, _) =>
          def tronInner(): Unit = {
            val more =
              try {
                if (traceChecks) {
                  val cname = "fun.opengl.GLUtil$"
                  val loc =
                    Option(Thread.currentThread.getStackTrace) match {
                      case Some(stack) =>
                        stack.dropWhile(_.getClassName != cname).dropWhile {
                          s => (s.getClassName == cname) && (s.getMethodName.startsWith("tron"))
                        }.take(1).map(s => " at " + s).mkString("\n")
                      case _ => " Unknown Call Location!"
                    }
                  log.info(s"Checked OpenGL State ($title):\n$loc")
                }
                Util.checkGLError
                false
              }
              catch {
                case ex: OpenGLException =>
                  report(ex, "GL ERROR")
                  found = true
                  true
              }
            if (more) tronInner()
          }
          tronInner()
        case _ =>
      }
      if (found) {
        debugLevel match {
          case GLDebugInitial | GLDebugPerFrame => setDebugLevel(GLDebugPerCall)
          case GLDebugPerCall if (abortOnError) => throw new OpenGLException("Aborting on Error!")
          case _                                =>
        }
      }
    }
    if (checkBefore) tronCheck("Before")
    val rtn = body
    if (checkAfter) tronCheck("After")
    rtn
  }

  /** Create a default OpenGL display.
    * @param isFullscreen Whether the display will be full-screen.
    * @param size The dimensions in pixels of the display area.
    * @param stencilBits The number of bits to allocate for stencil buffers.
    */
  def createDisplay(
    isFullscreen: Boolean = true,
    size: Option[Index2i] = None,
    stencilBits: Int = 0
  ): Unit = {
    val mode =
      (isFullscreen, size) match {
        case (false, Some(s)) => new DisplayMode(s.x, s.y)
        case (false, None) =>
          val m = Display.getDesktopDisplayMode
          val s = (Vec2d(m.getWidth.toDouble, m.getHeight.toDouble) * 0.75).toIndex2i
          new DisplayMode(s.x, s.y)
        case (true, _) => Display.getDesktopDisplayMode
      }
    log.info(s"Display Mode: [$mode]")
    if(isFullscreen) Display.setDisplayModeAndFullscreen(mode)
    else Display.setDisplayMode(mode)

    val pxf = new PixelFormat().withStencilBits(stencilBits)
    val cattrs = new ContextAttribs(3, 2).withForwardCompatible(true).withProfileCore(true)
    val dsp = Display.create(pxf, cattrs)
    tronInitial()
    dsp
  }

  /** Print OpenGL version information. */
  def printInfo(): Unit =
    log.info(s"OpenGL Version: ${glGetString(GL_VERSION)}")

  /** Print OpenGL extensions. */
  def printExtensions(): Unit =
    tron {
      val exts = glGetString(GL_EXTENSIONS).split(" ").sortWith(_.compareTo(_) < 0).mkString("\n  ")
      log.info(s"OpenGL Extensions:\n  $exts")
    }

  /** Basic OpenGL setup. */
  def setup(clearColor: Vec3d = Vec3d(1.0)): Unit = {
    tron { glClearColor(clearColor.x.toFloat, clearColor.y.toFloat, clearColor.z.toFloat, 1.0f) }
    tron { glEnable(GL_BLEND) }
    tron { glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA) }
    tron { glPolygonMode(GL11.GL_FRONT_AND_BACK, GL11.GL_FILL) }
  }

  /** Clear the color and depth buffers. */
  def clear(): Unit =
    tron {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    }
}
