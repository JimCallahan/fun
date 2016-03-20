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

import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.nio.file.Path
import java.nio.file.Paths

import org.lwjgl.opengl.GL11.GL_TRUE
import org.lwjgl.opengl.GL20.GL_COMPILE_STATUS
import org.lwjgl.opengl.GL20.GL_FRAGMENT_SHADER
import org.lwjgl.opengl.GL20.GL_INFO_LOG_LENGTH
import org.lwjgl.opengl.GL20.GL_LINK_STATUS
import org.lwjgl.opengl.GL20.GL_VERTEX_SHADER
import org.lwjgl.opengl.GL20.glAttachShader
import org.lwjgl.opengl.GL20.glCompileShader
import org.lwjgl.opengl.GL20.glCreateProgram
import org.lwjgl.opengl.GL20.glCreateShader
import org.lwjgl.opengl.GL20.glDeleteProgram
import org.lwjgl.opengl.GL20.glDeleteShader
import org.lwjgl.opengl.GL20.glDetachShader
import org.lwjgl.opengl.GL20.glGetProgramInfoLog
import org.lwjgl.opengl.GL20.glGetProgrami
import org.lwjgl.opengl.GL20.glGetShaderInfoLog
import org.lwjgl.opengl.GL20.glGetShaderi
import org.lwjgl.opengl.GL20.glGetUniformLocation
import org.lwjgl.opengl.GL20.glLinkProgram
import org.lwjgl.opengl.GL20.glShaderSource
import org.lwjgl.opengl.GL20.glUniform1f
import org.lwjgl.opengl.GL20.glUniform1i
import org.lwjgl.opengl.GL20.glUniform2f
import org.lwjgl.opengl.GL20.glUniform2i
import org.lwjgl.opengl.GL20.glUniform3f
import org.lwjgl.opengl.GL20.glUniform3i
import org.lwjgl.opengl.GL20.glUniformMatrix4
import org.lwjgl.opengl.GL20.glUseProgram
import org.lwjgl.opengl.GL32.GL_GEOMETRY_SHADER

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import fun.math.Index1i
import fun.math.Index2i
import fun.math.Index3i
import fun.math.Matrix44d
import fun.math.Pos1d
import fun.math.Pos2d
import fun.math.Pos3d
import fun.math.Vec1d
import fun.math.Vec2d
import fun.math.Vec3d
import fun.util.Logging

import GLUtil.tron

/** Types of GLSL shaders. */
sealed trait ShaderType
case object Vertex extends ShaderType
case object Geometry extends ShaderType
case object Fragment extends ShaderType

/** Factory for Shader. */
object Shader {
  /** The local file system directory containing shaders.
    * If None, then shaders will be read from the application JAR. */
  val shaderDir: Option[Path] =
    Try {
      Option(System.getProperty("shaderDir")) map {
        case str =>
          val path = Paths.get(str)
          require(path.toFile.isDirectory)
          path
      }
    }.toOption.flatten

  /** Create a new GLSL shader by loading and compiling its source code.
    * @param shaderType The type of GLSL shader.
    * @param path The path of the file within the application JAR containing the shader source
    * code. */
  def apply(shaderType: ShaderType, path: Path): Shader = {
    val stype = shaderType match {
      case Vertex   => GL_VERTEX_SHADER
      case Geometry => GL_GEOMETRY_SHADER
      case Fragment => GL_FRAGMENT_SHADER
    }
    new Shader(stype, path)
  }
}

/** A GLSL shader. */
class Shader(shaderType: Int, path: Path)
    extends Logging {

  val shaderID = {
    import java.io.{ BufferedReader, FileReader, InputStreamReader }
    val shader = tron { glCreateShader(shaderType) }
    if(shader == 0) {
      log.error("Failed to create GLSL shader!")
      0
    }
    else {
      Try {
        val in =
          Shader.shaderDir match {
            case Some(dir) =>
              new BufferedReader(new FileReader(dir.resolve(path).toFile))
            case None =>
              new BufferedReader(new InputStreamReader(getClass.getResourceAsStream(path.toString)))
          }
        try {
          @tailrec
          def f(all: String): String =
            Option(in.readLine) match {
              case None       => all
              case Some(line) => f(s"$all\n$line")
            }
          f("")
        }
        finally {
          in.close
        }
      } match {
        case Success(shaderSource) =>
          tron { glShaderSource(shader, shaderSource) }
          tron { glCompileShader(shader) }
          if (glGetShaderi(shader, GL_COMPILE_STATUS) == GL_TRUE) {
            log.info(s"GLSL shader compiled: $path")
            shader
          }
          else {
            log.error("GLSL shader compile failed:\n" +
              glGetShaderInfoLog(shader, glGetShaderi(shader, GL_INFO_LOG_LENGTH)))
            0
          }
        case Failure(ex) =>
          log.error(s"Unable to read GLSL shader source from: $path\n${ex.getMessage}")
          0
      }
    }
  }
}

/** Factory for ShaderProgram. */
object ShaderProgram {
  /** Create a new GLSL shader program by binding shader attributes and linking the shaders.
    * @param shaders List of GLSL shaders which make up the program.
    * @param unames The names of the uniform variables defined in the shaders. */
  def apply(shaders: List[Shader], unames: List[String]): ShaderProgram =
    new ShaderProgram(shaders, unames)
}

/** A set of GLSL shaders which together make up a shader program. */
class ShaderProgram(val shaders: List[Shader], unames: List[String])
    extends Logging {

  /** Link the shaders storing the shader program handle. */
  val programID: Int = {
    val pid = tron { glCreateProgram }
    if (pid == 0) {
      log.error("Failed to create GLSL shader program!")
      freeResources(pid)
    }
    else {
      // Attach the shaders to the program.
      for (shader <- shaders)
        tron { glAttachShader(pid, shader.shaderID) }

      // Link shaders to build the shader program.
      tron { glLinkProgram(pid) }
      if (glGetProgrami(pid, GL_LINK_STATUS) == GL_TRUE) {
        log.info("Shader program linked successfully.")
        pid
      }
      else {
        log.error("GLSL shader program link failed:\n" +
          glGetProgramInfoLog(pid, glGetProgrami(pid, GL_INFO_LOG_LENGTH)))
        freeResources(pid)
      }
    }
  }

  /** Installs a program object as part of current rendering state. */
  def use(): Unit =
    tron { glUseProgram(programID) }

  /** A map of uniform variable names to IDs. */
  val uniforms = {
    def f(m: Map[String, Int], u: String): Map[String,Int] =
      m.updated(u, tron { glGetUniformLocation(programID, u) })
    ((Map(): Map[String, Int]) /: unames)(f)
  }

  /** Set the value of an integer uniform shader variable. */
  def setUniform(name: String, i: Int): Unit =
    tron { glUniform1i(uniforms(name), i) }

  /** Set the value of a float uniform shader variable. */
  def setUniform(name: String, v: Float): Unit =
    tron { glUniform1f(uniforms(name), v) }

  /** Set the value of a double uniform shader variable. */
  def setUniform(name: String, v: Double): Unit =
    tron { glUniform1f(uniforms(name), v.toFloat) }

  /** Set the value of an 1-dimensional vector uniform shader variable. */
  def setUniform(name: String, v: Vec1d): Unit =
    tron { glUniform1f(uniforms(name), v.x.toFloat) }

  /** Set the value of an 2-dimensional vector uniform shader variable. */
  def setUniform(name: String, v: Vec2d): Unit =
    tron { glUniform2f(uniforms(name), v.x.toFloat, v.y.toFloat) }

  /** Set the value of an 3-dimensional vector uniform shader variable. */
  def setUniform(name: String, v: Vec3d): Unit =
    tron { glUniform3f(uniforms(name), v.x.toFloat, v.y.toFloat, v.z.toFloat) }

  /** Set the value of an 1-dimensional position uniform shader variable. */
  def setUniform(name: String, p: Pos1d): Unit =
    tron { glUniform1f(uniforms(name), p.x.toFloat) }

  /** Set the value of an 2-dimensional position uniform shader variable. */
  def setUniform(name: String, p: Pos2d): Unit =
    tron { glUniform2f(uniforms(name), p.x.toFloat, p.y.toFloat) }

  /** Set the value of an 3-dimensional position uniform shader variable. */
  def setUniform(name: String, p: Pos3d): Unit =
    tron { glUniform3f(uniforms(name), p.x.toFloat, p.y.toFloat, p.z.toFloat) }

  /** Set the value of an 1-dimensional index uniform shader variable. */
  def setUniform(name: String, idx: Index1i): Unit =
    tron { glUniform1i(uniforms(name), idx.x) }

  /** Set the value of an 2-dimensional index uniform shader variable. */
  def setUniform(name: String, idx: Index2i): Unit =
    tron { glUniform2i(uniforms(name), idx.x, idx.y) }

  /** Set the value of an 3-dimensional index uniform shader variable. */
  def setUniform(name: String, idx: Index3i): Unit =
    tron { glUniform3i(uniforms(name), idx.x, idx.y, idx.z) }

  /** Set the value of an 4x4 matrix uniform shader variable. */
  def setUniform(name: String, mx: Matrix44d): Unit =
    tron { mx.useNativeFloats { glUniformMatrix4(uniforms(name), false, _) } }

  /** Free up any OpenGL resources used by the shader program.
    *
    * Should the programID and Shader.shaderID be "vars" and not vals" to prevent this from being
    * called twice?  Also how should we cleanup resources after successfully using some shaders
    * but no longer needing them?
    *
    * @param pid The program handle.
    * @returns Always 0.
    */
  private def freeResources(pid: Int): Int = {
    for (shader <- shaders) {
      if (pid != 0)
        tron { glDetachShader(pid, shader.shaderID) }
      tron { glDeleteShader(shader.shaderID) }
    }
    if (pid != 0)
      tron { glDeleteProgram(pid) }
    0
  }
}
