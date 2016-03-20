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

import org.lwjgl.opengl.GL11.GL_TRIANGLES
import org.lwjgl.opengl.GL12.glDrawRangeElements

import fun.math.Pos2d
import fun.math.Pos3d
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
import fun.opengl.Material
import fun.opengl.Shader
import fun.opengl.ShaderProgram
import fun.opengl.TextureMgr
import fun.opengl.VectorFontTextures
import fun.opengl.VectorSampler
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera

import GLUtil.tron

object TestVectorTextApp
    extends GLApp {

  def appName: String = "TestVectorText"

  /** The complete graphics state.
    * @param direct The camera director.
    * @param font The font metrics.
    * @param mat The font material.
    * @param numGlyphs The number of quads to render.
    * @param textVAO The per-glyph quad geometry and attributes.
    */
  case class State(
    direct: Director2d,
    font: VectorFontTextures,
    mat: Material,
    numGlyphs: Int,
    textVAO: VertexArrayObject
  )
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
      mat.program.setUniform("MvpMatrix", ndirect.cam.xform)

      // Draw the geometry using VAOs.
      textVAO.bind
      tron {
        glDrawRangeElements(
          GL_TRIANGLES, 0,
          numGlyphs * 4, numGlyphs * 6,
          textVAO.ibo.dataType, 0
        )
      }

      copy(direct = ndirect)
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

    // "Henry V" battlefield speech
    val dialog = List(
      "This story shall the good man teach his son;",
      "And Crispin Crispian shall ne'er go by,",
      "From this day to the ending of the world,",
      "But we in it shall be remember'd;",
      "We few, we happy few, we band of brothers;",
      "For he to-day that sheds his blood with me",
      "Shall be my brother; be he ne'er so vile,",
      "This day shall gentle his condition:",
      "And gentlemen in England now a-bed",
      "Shall think themselves accursed they were not here,",
      "And hold their manhoods cheap whiles any speaks",
      "That fought with us upon Saint Crispin's day.")

    // Textures quads, one per character.
    val (numGlyphs, textVAO) = {
      // layout the characters of the dialog: (glyph index, anchor position)
      val layout =
        (List[(Int, Pos2d)]() /: dialog.zipWithIndex) {
          case (ls, (line, idx)) =>
            val (lineLayout, _) =
              ((ls, Pos2d(0.0, -1.5 * idx.toDouble)) /: line) {
                case (rtn @ (lls, anchor), c) =>
                  font.glyphs.get(c) match {
                    case Some(g) =>
                      ((g.index, anchor) :: lls, anchor + Vec2d(0.1 + g.advance, 0.0))
                    case None if (c == ' ') =>
                      (lls, anchor + Vec2d(0.35, 0.0))
                    case _ => rtn
                  }
              }
            lineLayout
        }.reverse.toIndexedSeq

      val size = layout.size

      // 4 verts, 2 XY
      val verts =
        FloatVBO(size * 4, 2) {
          case buf =>
            val corners = List(Vec2d(-1.0), Vec2d(-1.0, 1.0), Vec2d(1.0, -1.0), Vec2d(1.0))
            for {
              (_, anchor) <- layout
              v <- corners
            } (anchor + v) >>> buf
        }

      // 4 verts, 3 UVW
      val coords =
        FloatVBO(size * 4, 3) {
          case buf =>
            val corners = List(Vec2d(0.0), Vec2d(0.0, 1.0), Vec2d(1.0, 0.0), Vec2d(1.0))
            for {
              (gi, _) <- layout
              v <- corners
            } (Pos3d(0.0, 0.0, gi.toDouble) + v.toVec3d) >>> buf
        }

      // 2 tris, 3 indices
      val indices =
        IntIBO(size * 2 * 3) {
          case buf =>
            for {
              i <- 0 until size
              j = i * 4
            } {
              buf.put(j + 0)
              buf.put(j + 1)
              buf.put(j + 2)

              buf.put(j + 2)
              buf.put(j + 1)
              buf.put(j + 3)
            }
        }

      (size, VertexArrayObject(List(verts, coords), indices))
    }

    //---------------------------------------------------------------------------------------------

    val direct = Director2d(cam)
    State(direct, font, mat, numGlyphs, textVAO)
  }
}
