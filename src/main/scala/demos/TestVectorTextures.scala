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
import fun.opengl.TextureMgr
import fun.opengl.VectorSampler
import fun.opengl.VectorTexture2D
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera

import GLUtil.tron

object TestVectorTexturesApp
    extends GLApp {

  def appName: String = "TestVectorTextures"

  /** The complete graphics state.
    * @param direct The camera director.
    * @param renderer The index of the shader program being used (0=simple, 1=anti-aliased)
    * @param geometry The index of the geometry being used (0=dragon, 1=griffin)
    * @param materials The materials (0=simple-dragon, 1=simple-griffin, 2=aa-dragon, 3=aa-griffin)
    * @param simpleVAO The single quad geometry and its attributes.
    */
  case class State(
    direct: Director2d,
    renderer: Int,
    geometry: Int,
    materials: Array[Material],
    simpleVAO: VertexArrayObject
  )
      extends GraphicsState {

    def render: GraphicsState = {
      val cameras = Map("MainCam" -> direct.cam.xform)
      val cameraNames = cameras.keys.toIndexedSeq
      val input = Input(displaySize, cameras)

      // Update camera.
      val (ndirect, ninput) = direct.react(input)

      // Change which render/geometry settings to use.
      val (ri, gi) =
        ((renderer, geometry) /: ninput.keyboard) {
          case ((r, g), KeyboardEvent(_, _, k, Pressed, ShiftUp, ControlUp, MetaUp)) =>
            k match {
              case Keyboard.KEY_1 => (0, g)
              case Keyboard.KEY_2 => (1, g)
              case Keyboard.KEY_3 => (r, 0)
              case Keyboard.KEY_4 => (r, 1)
              case _                 => (r, g)
            }
          case ((r, g), _) => (r, g)
        }

      // Clear the screen.
      GLUtil.clear()

      // Choose the material.
      val mat = materials(gi + 2 * ri)
      mat.bind

      // Set Model-View-Projection matrix.
      mat.program.setUniform("MvpMatrix", ndirect.cam.xform)

      // Draw the geometry using VAOs.
      simpleVAO.bind
      tron { glDrawRangeElements(GL_TRIANGLES, 0, 6, 6, simpleVAO.ibo.dataType, 0) }

      copy(direct = ndirect, renderer = ri, geometry = gi)
    }
  }

  /** Create the initial the graphics state. */
  def initial: State = {
    // Initialize OpenGL.
    GLUtil.setDebugLevel(GLUtil.GLDebugPerCall)
    GLUtil.setup(Vec3d(0.0))
    tron { glEnable(GL_DEPTH_TEST) }

    // Cameras.
    val cam = ZoomCamera(displaySize, 0.0)

    //---------------------------------------------------------------------------------------------

    // Textures/Samplers
    val textures = {
      val names = Array("griffin", "dragon")
      for (n <- names)
        TextureMgr.add(n, VectorTexture2D(Paths.get(s"/demos/textures/$n-vt.png")), VectorSampler())
      names
    }

    //---------------------------------------------------------------------------------------------

    val dir = Paths.get("/demos/shaders")

    // Load and compile shaders.
    val progs = {
      Array(
        ShaderProgram(
          List(
            Shader(Vertex, dir.resolve("texture.vert")),
            Shader(Fragment, dir.resolve("vector-texture.frag"))
          ),
          List("MvpMatrix", "Texture")
        ),
        ShaderProgram(
          List(
            Shader(Vertex, dir.resolve("texture.vert")),
            Shader(Fragment, dir.resolve("aa-vector-texture.frag"))
          ),
          List("MvpMatrix", "Texture")
        )
      )
    }

    // Assemble materials.
    val materials =
      for {
        p <- progs
        t <- textures
      } yield Material(p, Map(t -> "Texture"))

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

    val direct = Director2d(cam)
    State(direct, 0, 0, materials, simpleVAO)
  }
}
