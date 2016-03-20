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
package demos

import java.nio.file.Paths

import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11.GL_DEPTH_TEST
import org.lwjgl.opengl.GL11.GL_TRIANGLES
import org.lwjgl.opengl.GL11.glEnable
import org.lwjgl.opengl.GL12.glDrawRangeElements

import fun.math.Vec2d
import fun.math.Vec3d
import fun.opengl.ClampedMipmapSampler
import fun.opengl.Director2d
import fun.opengl.FloatVBO
import fun.opengl.Fragment
import fun.opengl.GLApp
import fun.opengl.GLUtil
import fun.opengl.GraphicsState
import fun.opengl.Input
import fun.opengl.IntIBO
import fun.opengl.KeyboardEvent
import fun.opengl.KeyboardEvent.ControlUp
import fun.opengl.KeyboardEvent.MetaUp
import fun.opengl.KeyboardEvent.Pressed
import fun.opengl.KeyboardEvent.ShiftUp
import fun.opengl.Material
import fun.opengl.Shader
import fun.opengl.ShaderProgram
import fun.opengl.TestTexture2D
import fun.opengl.TextureMgr
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera

import GLUtil.tron

object TestTexturesApp
    extends GLApp {

  def appName: String = "TestTextures"

  sealed trait RenderMode
  case object Simple extends RenderMode
  case object Random extends RenderMode

  /** The complete graphics state. */
  case class State(
    direct: Director2d,
    renderMode: RenderMode,
    simpleMat: Material,
    coloredMat: Material,
    simpleVAO: VertexArrayObject,
    numRandomQuads: Int,
    randomVAO: VertexArrayObject
  ) extends GraphicsState {

    def render: GraphicsState = {
      val cameras = Map("MainCam" -> direct.cam.xform)
      val cameraNames = cameras.keys.toIndexedSeq
      val input = Input(displaySize, cameras)

      // Update camera.
      val (ndirect, ninput) = direct.react(input)

      // Clear the screen.
      GLUtil.clear()

      // Change which geometry to display.
      val nrenderMode =
        (renderMode /: ninput.keyboard) {
          case (rm, KeyboardEvent(_, _, k, Pressed, ShiftUp, ControlUp, MetaUp)) =>
            k match {
              case Keyboard.KEY_1 => Simple
              case Keyboard.KEY_2 => Random
              case _ => rm
            }
          case (rm, _) => rm
        }

      // Choose the material to use.
      val mat =
        renderMode match {
          case Simple => simpleMat
          case Random => coloredMat
        }
      mat.bind

      // Set Model-View-Projection matrix.
      mat.program.setUniform("MvpMatrix", ndirect.cam.xform)

      // Draw the geometry using VAOs.
      nrenderMode match {
        case Simple => {
          simpleVAO.bind
          tron { glDrawRangeElements(GL_TRIANGLES, 0, 6, 6, simpleVAO.ibo.dataType, 0) }
        }

        case Random => {
          randomVAO.bind
          val size = numRandomQuads * 6
          tron { glDrawRangeElements(GL_TRIANGLES, 0, size, size, randomVAO.ibo.dataType, 0) }
        }
      }

      copy(direct = ndirect, renderMode = nrenderMode)
    }
  }

  /** Create the initial the graphics state. */
  def initial: State = {
    // Initialize OpenGL.
    GLUtil.setDebugLevel(GLUtil.GLDebugPerCall)
    GLUtil.setup()
    tron { glEnable(GL_DEPTH_TEST) }

    // Cameras.
    val cam = ZoomCamera(displaySize, 0.0)

    //---------------------------------------------------------------------------------------------

    // Texture and sampler.
    TextureMgr.add("Testing", TestTexture2D(1024), ClampedMipmapSampler())

    //---------------------------------------------------------------------------------------------

    val dir = Paths.get("/demos/shaders")

    // Create materials.
    val simpleMat = {
      val prog = ShaderProgram(
        List(
          Shader(Vertex, dir.resolve("texture.vert")),
          Shader(Fragment, dir.resolve("texture.frag"))
        ),
        List("MvpMatrix", "Texture")
      )
      Material(prog, Map("Testing" -> "Texture"))
    }

    val coloredMat = {
      val prog = ShaderProgram(
        List(
          Shader(Vertex, dir.resolve("colored-texture.vert")),
          Shader(Fragment, dir.resolve("colored-texture.frag"))
        ),
        List("MvpMatrix", "Texture")
      )
      Material(prog, Map("Testing" -> "Texture"))
    }

    //---------------------------------------------------------------------------------------------

    // One texture mapped quad.
    val simpleVAO = {
      // 4 verts, 2 XY
      val verts =
        FloatVBO(4, 2) {
          case buf =>
            Vec2d(-1.0) >>> buf
            Vec2d(-1.0, 1.0) >>> buf
            Vec2d(1.0, -1.0) >>> buf
            Vec2d(1.0) >>> buf
        }

      // 4 verts, 2 UV
      val coords =
        FloatVBO(4, 2) {
          case buf =>
            Vec2d(0.0) >>> buf
            Vec2d(0.0, 1.0) >>> buf
            Vec2d(1.0, 0.0) >>> buf
            Vec2d(1.0) >>> buf
        }

      // 2 tris, 3 indices
      val indices =
        IntIBO(2 * 3) {
          case buf =>
            buf.put(0)
            buf.put(1)
            buf.put(2)

            buf.put(2)
            buf.put(1)
            buf.put(3)
        }

      VertexArrayObject(List(verts, coords), indices)
    }

    // A number of randomly placed and sized texture mapped quads.
    val numRandomQuads = 4096
    val randomVAO = {
      // 4 verts, 2 XY
      val verts =
        FloatVBO(numRandomQuads * 4, 2) {
          case buf =>
            val bbox = cam.camera.bbox.toBBox2d
            val size = bbox.range.x * 0.001
            for (i <- 0 until numRandomQuads) {
              val p = bbox.randomPos
              val d = Vec2d(size + size * scala.math.random * 32.0)
              (p - d) >>> buf
              (p + d.updateY(0.0) - d.updateX(0.0)) >>> buf
              (p - d.updateY(0.0) + d.updateX(0.0)) >>> buf
              (p + d) >>> buf
            }
        }

      // 4 verts, 2 UV
      val coords =
        FloatVBO(numRandomQuads * 4, 2) {
          case buf =>
            for (_ <- 0 until numRandomQuads) {
              Vec2d(0.0) >>> buf
              Vec2d(0.0, 1.0) >>> buf
              Vec2d(1.0, 0.0) >>> buf
              Vec2d(1.0) >>> buf
            }
        }

      // 4 verts, 3 RGB
      val colors =
        FloatVBO(numRandomQuads * 4, 3) {
          case buf =>
            for (_ <- 0 until numRandomQuads) {
              val c = Vec3d.random()
              for (_ <- 0 until 4)
                c >>> buf
            }
        }

      // 2 tris, 3 indices
      val indices =
        IntIBO(numRandomQuads * 2 * 3) {
          case buf =>
            for (i <- 0 until numRandomQuads) {
              buf.put(i * 4 + 0)
              buf.put(i * 4 + 1)
              buf.put(i * 4 + 2)

              buf.put(i * 4 + 3)
              buf.put(i * 4 + 2)
              buf.put(i * 4 + 1)
            }
        }

      VertexArrayObject(List(verts, coords, colors), indices)
    }

    //---------------------------------------------------------------------------------------------

    val direct = Director2d(cam)
    State(direct, Simple, simpleMat, coloredMat, simpleVAO, numRandomQuads, randomVAO)
  }
}
