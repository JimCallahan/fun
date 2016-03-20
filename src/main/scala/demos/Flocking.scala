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
import org.lwjgl.opengl.GL11.GL_LINES
import org.lwjgl.opengl.GL11.GL_POINTS
import org.lwjgl.opengl.GL11.glEnable
import org.lwjgl.opengl.GL12.glDrawRangeElements
import org.lwjgl.opengl.GL15.GL_DYNAMIC_DRAW

import fun.math.BBox2d
import fun.math.Pos2d
import fun.math.Scalar
import fun.math.Vec2d
import fun.math.Vec3d
import fun.opengl.Director2d
import fun.opengl.FloatVBO
import fun.opengl.Fragment
import fun.opengl.GLApp
import fun.opengl.GLAppLike
import fun.opengl.GLUtil
import fun.opengl.GLUtil.tron
import fun.opengl.GraphicsState
import fun.opengl.Input
import fun.opengl.IntIBO
import fun.opengl.Shader
import fun.opengl.ShaderProgram
import fun.opengl.Vertex
import fun.opengl.VertexArrayObject
import fun.opengl.ZoomCamera

import GLUtil.tron

object FlockingApp
    extends GLApp {

  def appName: String = "Flocking"

  /** A creature.
    * @param pos Its position.
    * @param vel Its velocity per second.
    */
  case class Creature(pos: Pos2d, vel: Vec2d)

  /** The complete graphics state.
    * @param frameDuration The duration of an animation frame in seconds.
    * @param direct The directed camera.
    * @param numPrey The original number of prey creatures.
    * @param prey The prey creatures.
    * @param preyColor The main color of prey.
    * @param numPred The original number of predator creatures.
    * @param pred The predator creatures.
    * @param predColor The main color of predators.
    * @param prog The GL shader program.
    * @param verts The dynamic position/velocity data.
    * @param colors The dynamic color data.
    * @param pointsVAO The point geometry and its attributes.
    * @param tailsVAO The tail geometry and its attributes.
    */
  case class State(
    frameDuration: Double,
    direct: Director2d,
    numPrey: Int,
    prey: Array[Creature],
    preyColor: Vec3d,
    numPred: Int,
    pred: Array[Creature],
    predColor: Vec3d,
    prog: ShaderProgram,
    verts: FloatVBO,
    colors: FloatVBO,
    pointsVAO: VertexArrayObject,
    tailsVAO: VertexArrayObject
  )
      extends GraphicsState {

    /** Gaussian function that is one at origin and approximately zero at radius. */
    private def gauss(dist: Double, radius: Double) = {
      import scala.math.{ E, pow }
      pow(E, -1.0 * pow((dist / radius) * 2.0, 2.0))
    }

    /** Spring function that is positive one at the origin, zero at radius, negative one at
      * three radii and approximately zero again at five radii and beyond. */
    private def spring(dist: Double, radius: Double) =
      gauss(dist, radius) - gauss(dist - radius * 3.0, radius * 2.0)

    /** Render the given graphics state while generating the state for the next frame. */
    def render: GraphicsState = {
      val cameras = Map("MainCam" -> direct.cam.xform)
      val cameraNames = cameras.keys.toIndexedSeq
      val input = Input(displaySize, cameras)

      // Update camera.
      val (ndirect, ninput) = direct.react(input)

      // Clear the screen.
      GLUtil.clear()

      // Reset the simulation?
      val isReset = ninput.keyboard exists { _.isPressed }

      // Simulation constants.
      val flock = 0.01
      val flockRadius = 5.0

      val flee = 0.05
      val fleeRadius = 5.0

      val kill = 0.1
      val killRadius = 0.1

      val maxVel = 10.0
      val damper = 0.99

      // Animate the prey.
      val nprey =
        if(isReset) {
          val bbox = direct.cam.bbox.toBBox2d
          val diag = Pos2d(bbox.range.mag)
          val sbox = BBox2d(diag * -1.0, diag)
          Array.fill(numPrey)(Creature(pos = sbox.randomPos, vel = Vec2d.zero))
        }
        else {
          prey.map {
            p =>
            val flockForce = (Vec2d.zero /: prey) {
              (fc, q) =>
              val v = p.pos - q.pos
              val m = v.mag
              if (Scalar.equiv(m, 0.0)) fc
              else fc + (v / m) * spring(m, flockRadius)
            }

            val fleeForce = (Vec2d.zero /: pred) {
              (fc, q) =>
              val v = p.pos - q.pos
              val m = v.mag
              if (Scalar.equiv(m, 0.0)) fc
              else fc + (v / m) * gauss(m, fleeRadius)
            }

            val vel = {
              val v = p.vel * damper + (flockForce * flock) + (fleeForce * flee)
              val m = v.mag
              if (Scalar.equiv(m, 0.0)) Vec2d.zero
              else if (m < maxVel) v
              else (v / m) * maxVel
            }

            val pos = p.pos + vel * frameDuration

            Creature(pos, vel)
          } filter {
            p => pred.forall(q => (q.pos - p.pos).mag > killRadius)
          }
        }

      val npred =
        if(isReset) {
          val bbox = direct.cam.camera.bbox.toBBox2d
          val diag = Pos2d(bbox.range.mag)
          val sbox = BBox2d(diag * -1.0, diag)
          Array.fill(numPred)(Creature(pos = sbox.randomPos, vel = Vec2d.zero))
        }
        else {
          pred.map {
            p =>
            val (qm, qv) = ((Double.MaxValue, Vec2d.zero) /: nprey) {
              case ((mm, mv), q) =>
                val v = q.pos - p.pos
                val m = v.mag
                if (m < mm) (m, v) else (mm, mv)
            }

            val dir =
              if (Scalar.equiv(qm, 0.0)) Vec2d.zero
              else (qv / qm)

            val vel = p.vel * damper + dir * kill

            p.copy(pos = p.pos + vel * frameDuration, vel = vel * damper)
          }
        }

      // Update the prey position/color buffers.
      val numCreatures = nprey.size + npred.size
      verts.reload(0L) {
        case buf =>
          for (p <- nprey) {
            p.pos >>> buf
            (p.pos - p.vel * 0.2) >>> buf
          }
          for (p <- npred) {
            p.pos >>> buf
            (p.pos - p.vel * 0.2) >>> buf
          }
      }
      colors.reload(0L) {
        case buf =>
          val preyTail = Vec3d(1.0).lerp(preyColor, 0.5)
          for (p <- nprey) {
            preyColor >>> buf
            preyTail >>> buf
          }
          val predTail = Vec3d(1.0).lerp(predColor, 0.5)
          for (p <- npred) {
            predColor >>> buf
            predTail >>> buf
          }
      }

      // Set Model-View-Projection matrix.
      tron {
        prog.setUniform("MvpMatrix", ndirect.cam.xform)
      }

      // Draw the prey tails.
      tailsVAO.bind
      tron {
        glDrawRangeElements(GL_LINES, 0, numCreatures * 2, numCreatures * 2, tailsVAO.ibo.dataType, 0)
      }

      // Draw the prey points.
      pointsVAO.bind
      tron {
        glDrawRangeElements(GL_POINTS, 0, numCreatures, numCreatures, pointsVAO.ibo.dataType, 0)
      }

      copy(direct = ndirect, prey = nprey, pred = npred)
    }
  }

  /** Create the initial the graphics state. */
  def initial: State = {
    // Initialize OpenGL.
    GLUtil.setDebugLevel(GLUtil.GLDebugPerCall)
    GLUtil.setup(Vec3d(0.95))
    tron { glEnable(GL_DEPTH_TEST) }

    // Camera.
    val cam = ZoomCamera(displaySize, 0.0)

    // Initialize creatures.
    val numPrey = 250
    val numPred = 8
    val numCreatures = numPrey + numPred
    val (prey, pred) = {
      val bbox = cam.camera.bbox.toBBox2d
      val diag = Pos2d(bbox.range.mag)
      val sbox = BBox2d(diag * -1.0, diag)
      val py = Array.fill(numPrey)(Creature(sbox.randomPos, Vec2d.zero))
      val pd = Array.fill(numPred)(Creature(sbox.randomPos, Vec2d.zero))
      (py, pd)
    }

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

    // Duration of an animation frame in seconds.
    val frameDuration = 1.0 / 60.0

    // Color palette.
    val preyColor = Vec3d(0.5, 0.6, 1.0)
    val predColor = Vec3d(1.0, 0.5, 0.2)

    // Create the prey points.
    val (verts, colors, pointsVAO, tailsVAO) = {
      // 2 verts, 2 XY
      val verts = FloatVBO(numCreatures * 2, 2, GL_DYNAMIC_DRAW)(FloatVBO.Unspecified)

      // 2 verts, 3 RGB
      val colors = FloatVBO(numCreatures * 2, 3, GL_DYNAMIC_DRAW)(FloatVBO.Unspecified)

      // N creatures, 1 point, 1 index
      val pointIndices =
        IntIBO(numCreatures) {
          case buf =>
            for (i <- 0 until numCreatures)
              buf.put(i * 2)
        }

      // N creatures, 1 line, 2 indices
      val tailIndices =
        IntIBO(numCreatures * 2) {
          case buf =>
            for (i <- 0 until numCreatures) {
              buf.put(i * 2 + 0)
              buf.put(i * 2 + 1)
            }
        }

      (verts, colors,
        VertexArrayObject(List(verts, colors), pointIndices),
        VertexArrayObject(List(verts, colors), tailIndices))
    }

    // Set shader program.
    prog.use

    val direct = Director2d(cam)

    State(
      frameDuration, direct,
      numPrey, prey, preyColor,
      numPred, pred, predColor,
      prog, verts, colors, pointsVAO, tailsVAO
    )
  }
}
