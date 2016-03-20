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

import org.lwjgl.opengl.GL11.GL_DEPTH_TEST
import org.lwjgl.opengl.GL11.GL_TRIANGLES
import org.lwjgl.opengl.GL11.glEnable
import org.lwjgl.opengl.GL12.glDrawRangeElements

import fun.math.BBox2d
import fun.math.Matrix44d
import fun.math.Pos2d
import fun.math.Vec2d
import fun.math.Vec3d
import fun.opengl.Director2d
import fun.opengl.FloatVBO
import fun.opengl.Fragment
import fun.opengl.GLApp
import fun.opengl.GLAppLike
import fun.opengl.GLUtil
import fun.opengl.GraphicsState
import fun.opengl.Input
import fun.opengl.IntIBO
import fun.opengl.Shader
import fun.opengl.ShaderProgram
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera

import GLUtil.tron

object TestVboShadersApp
    extends GLApp {

  def appName: String = "TestVboShaders"

  /** The complete graphics state.
    * @param direct The camera director.
    * @param prog The GL shader program.
    * @param numQuads The number of quadrilaterals.
    * @param vao The geometry and its attributes.
    * @param ang The animation rotation angle.
    */
  case class State(direct: Director2d,
    prog: ShaderProgram,
    numQuads: Int,
    vao: VertexArrayObject,
    ang: Double)
      extends GraphicsState {

    def render: GraphicsState = {
      val cameras = Map("MainCam" -> direct.cam.xform)
      val cameraNames = cameras.keys.toIndexedSeq
      val input = Input(displaySize, cameras)

      // Update camera.
      val (ndirect, _) = direct.react(input)

      // Clear the screen.
      GLUtil.clear()

      // Set Model-View-Projection matrix.
      val nang = ang + scala.math.Pi * 0.001
      prog.setUniform("MvpMatrix", ndirect.cam.xform * Matrix44d.rotateZ(nang))

      // Draw the triangle using VAO.
      vao.bind
      tron { glDrawRangeElements(GL_TRIANGLES, 0, numQuads * 6, numQuads * 6, vao.ibo.dataType, 0) }

      copy(direct = ndirect, ang = nang)
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

    // Load and compile shaders.
    val prog = {
      val dir = Paths.get("/demos/shaders")
      ShaderProgram(
        List(
          Shader(Vertex, dir.resolve("simple.vert")),
          Shader(Fragment, dir.resolve("simple.frag"))
        ),
        List("MvpMatrix")
      )
    }

    // Create some VBO data: random 2D quads with colors.
    val numQuads = 100000
    val vao = {
      // 4 verts, 2 attrs XY
      val verts =
        FloatVBO(numQuads * 4, 2) {
          case buf =>
            val bbox = cam.camera.bbox.toBBox2d
            val diag = Pos2d(bbox.range.mag)
            val sbox = BBox2d(diag * -1.0, diag)
            val size = bbox.range.x * 0.005
            val delta = Vec2d(size)
            val dx = delta.updateY(0.0)
            val dy = delta.updateX(0.0)
            for (i <- 0 until numQuads) {
              val p = sbox.randomPos
              (p - delta) >>> buf
              (p + dx - dy) >>> buf
              (p - dx + dy) >>> buf
              (p + delta) >>> buf
            }
        }

      // 4 verts, 3 RGB
      val colors =
        FloatVBO(numQuads * 4, 3) {
          case buf =>
            import scala.math.random
            for (i <- 0 until numQuads) {
              val c = i.toDouble / numQuads.toDouble
              Vec3d(0.0, 0.0, c) >>> buf
              Vec3d(c, 0.0, 0.0) >>> buf
              Vec3d(0.0, c, 0.0) >>> buf
              Vec3d(c, c, 0.0) >>> buf
            }
        }

      // 2 tris, 3 indices
      val indices =
        IntIBO(numQuads * 2 * 3) {
          case buf =>
            for (i <- 0 until numQuads) {
              buf.put(i * 4 + 0)
              buf.put(i * 4 + 1)
              buf.put(i * 4 + 2)

              buf.put(i * 4 + 3)
              buf.put(i * 4 + 2)
              buf.put(i * 4 + 1)
            }
        }

      VertexArrayObject(List(verts, colors), indices)
    }

    // Set shader program.
    prog.use

    val direct = Director2d(cam)
    State(direct, prog, numQuads, vao, 30.0)
  }
}
