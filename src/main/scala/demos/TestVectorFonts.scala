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
import org.lwjgl.opengl.GL15.GL_DYNAMIC_DRAW

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
import fun.opengl.KeyboardEvent
import fun.opengl.KeyboardEvent.ControlUp
import fun.opengl.KeyboardEvent.MetaUp
import fun.opengl.KeyboardEvent.Pressed
import fun.opengl.KeyboardEvent.ShiftUp
import fun.opengl.Material
import fun.opengl.Shader
import fun.opengl.ShaderProgram
import fun.opengl.TextureMgr
import fun.opengl.VectorFontTextures
import fun.opengl.VectorSampler
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera
import fun.util.Logging

import GLUtil.tron

object TestVectorFontsApp
    extends GLApp
    with Logging {

  def appName: String = "TestVectorFonts"

  /** The complete graphics state.
    * @param direct The camera director.
    * @param character The character being displayed.
    * @param font The font metrics.
    * @param mat The font material.
    * @param coords The dynamic texture coordinates of the quad.
    * @param simpleVAO The single quad geometry and its attributes.
    */
  case class State(
    direct: Director2d,
    character: Char,
    font: VectorFontTextures,
    mat: Material,
    coords: FloatVBO,
    simpleVAO: VertexArrayObject
  )
      extends GraphicsState {

    def render: GraphicsState = {
      val cameras = Map("MainCam" -> direct.cam.xform)
      val cameraNames = cameras.keys.toIndexedSeq
      val input = Input(displaySize, cameras)

      // Update camera.
      val (ndirect, ninput) = direct.react(input)

      // Clear the screen.
      GLUtil.clear()

      // Check for a change of character to display.
      val ncharacter =
        (character /: ninput.keyboard) {
          case (c, KeyboardEvent(_, nc, _, Pressed, _, ControlUp, MetaUp))
              if(font.glyphs.contains(nc)) => nc
          case (c, _) => c
        }

      // Set texture coordinates to match glyph.
      coords.reload(0L) {
        case buf =>
          font.glyphs.get(ncharacter) match {
            case Some(g) =>
              val idx = Vec3d(0.0, 0.0, g.index.toDouble)
              for (c <- List(Vec2d(0.0), Vec2d(0.0, 1.0), Vec2d(1.0, 0.0), Vec2d(1.0)))
                (c.toVec3d + idx) >>> buf
            case _ => log.warn(s"Unknown Character: '$ncharacter'")
          }
      }

      // Set Model-View-Projection matrix.
      mat.program.setUniform("MvpMatrix", ndirect.cam.xform)

      // Draw the geometry using VAOs.
      simpleVAO.bind
      tron { glDrawRangeElements(GL_TRIANGLES, 0, 6, 6, simpleVAO.ibo.dataType, 0) }

      copy(direct = ndirect, character = ncharacter)
    }
  }

  /** Create the initial the graphics state. */
  def initial: State = {
    // Initialize OpenGL.
    GLUtil.setDebugLevel(GLUtil.GLDebugPerCall)
    GLUtil.setup(Vec3d(0.95))

    // Cameras.
    val cam = ZoomCamera(displaySize, 0.0)

    //---------------------------------------------------------------------------------------------

    // Font metrics and glyph textures
    val fontDir = Paths.get("/fonts/cantarell/Cantarell-Regular")
    val font = VectorFontTextures.load(fontDir)

    // Font texture and sampler.
    TextureMgr.add("VectorFont", font.loadTextures(fontDir), VectorSampler())

    //---------------------------------------------------------------------------------------------

    val dir = Paths.get("/demos/shaders")

    // Create the material.
    val mat = {
      val prog = ShaderProgram(
        List(
          Shader(Vertex, dir.resolve("font-texture.vert")),
          Shader(Fragment, dir.resolve("color-font-texture.frag"))
        ),
        List("MvpMatrix", "Texture", "FgColor", "BgColor")
      )
      Material(prog, Map("VectorFont" -> "Texture"))
    }

    // Bind the material now, since there is only one.
    mat.bind

    // Set the constant shader uniform variables.
    mat.program.setUniform("FgColor", Vec3d(0.35))
    mat.program.setUniform("BgColor", Vec3d(0.95))

    //---------------------------------------------------------------------------------------------

    // One texture mapped quad.
    val (simpleCoords, simpleVAO) = {
      // 4 verts, 2 XY
      val verts =
        FloatVBO(4, 2) {
          case buf =>
            Vec2d(-1.0) >>> buf
            Vec2d(-1.0, 1.0) >>> buf
            Vec2d(1.0, -1.0) >>> buf
            Vec2d(1.0) >>> buf
        }

      // 4 verts, 3 UVW
      val coords = FloatVBO(4, 3, GL_DYNAMIC_DRAW)(FloatVBO.Unspecified)

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

      (coords, VertexArrayObject(List(verts, coords), indices))
    }

    val direct = Director2d(cam)
    State(direct, 'A', font, mat, simpleCoords, simpleVAO)
  }
}
