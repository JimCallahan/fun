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

import fun.math.Interval
import fun.math.Pos2d
import fun.math.Scalar
import fun.util.Logging

/** Directs the position and zoom of an orthogonal camera based on user input. */
case class Director2d(cam: ZoomCamera, drag: Option[Pos2d])
    extends Logging {
  import Director2d.{ ZoomFactor, ZoomRange }

  def react(input: Input): (Director2d, Input) = {
    import MouseBindings.firstBtn
    val (ncam, ndrag, nmouse) = ((cam, drag, List[MouseEvent]()) /: input.mouse) {
      case ((c, d, es), e) =>
        val mp = e.pos("Device")
          (d, e) match {
          case (None, MouseButton(_, _, mbn, true)) if (mbn == firstBtn) => (c, Some(mp), es)
          case (Some(a), _: MouseMove) =>
            val delta = mp - a
            val npos = cam.pos + (delta * cam.factor) / scala.math.pow(2.0, cam.zoom)
            (c.copy(pos = npos), Some(mp), e :: es)
          case (Some(a), MouseButton(_, _, mbn, false)) if (mbn == firstBtn) => (c, None, es)
          case (_, MouseWheel(_, _, w)) if (w != 0) =>
            val nzoom = Scalar.clamp(cam.zoom+w.toDouble*ZoomFactor, ZoomRange.lower, ZoomRange.upper)
            (c.copy(zoom = nzoom), d, es)
          case _ => (c, d, e :: es)
        }
    }

    val ninput = input.copy(mouse = nmouse.reverse)
    val ndir = copy(cam = ncam, drag = ndrag)
    (ndir, ninput)
  }
}

object Director2d {
  /** Scaling factor of wheel movement to zoom delta. */
  private val ZoomFactor = 10e-4

  /** The range of allowable zoom levels. */
  private val ZoomRange = Interval(-15.0, 15.0)

  /** Create a new Director. */
  def apply(cam: ZoomCamera): Director2d =
    new Director2d(cam, None)
}
