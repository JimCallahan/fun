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

import fun.math.Frame2d
import fun.math.Pos2d

/** A mouse event. */
trait MouseEvent {
  /** The time in nanoseconds of the current event. */
  val nanos: Long

  /** The position of the mouse at the time of the event in each named camera space. */
  val pos: Map[String, Pos2d]

  /** A copy of the event with transformed position. */
  def xform(fr: Frame2d): MouseEvent
}

/** The current mouse position.
  * @param nanos The time in nanoseconds of the current event.
  * @param pos The position of the mouse at the time of the event in each named camera space.
  */
case class MouseCurrent(nanos: Long, pos: Map[String, Pos2d])
    extends MouseEvent {
  def xform(fr: Frame2d): MouseEvent = copy(pos = pos.mapValues(fr xform _))
}

/** A mouse event that involves a change in button state.
  * @param nanos The time in nanoseconds of the current event.
  * @param pos The position of the mouse at the time of the event in each named camera space.
  * @param buttonID The button that changed.
  * @param isPressed Whether it is now pressed.
  */
case class MouseButton(nanos: Long, pos: Map[String, Pos2d], buttonID: Int, isPressed: Boolean)
    extends MouseEvent {
  def xform(fr: Frame2d): MouseEvent = copy(pos = pos.mapValues(fr xform _))
}

/** A mouse event that only involves movement.
  * @param nanos The time in nanoseconds of the current event.
  * @param pos The position of the mouse at the time of the event in each named camera space.
  */
case class MouseMove(nanos: Long, pos: Map[String, Pos2d])
    extends MouseEvent {
  def xform(fr: Frame2d): MouseEvent = copy(pos = pos.mapValues(fr xform _))
}

/** A mouse event that only involves movement.
  * @param nanos The time in nanoseconds of the current event.
  * @param pos The position of the mouse at the time of the event in each named camera space.
  */
case class MouseWheel(nanos: Long, pos: Map[String, Pos2d], wheel: Int)
    extends MouseEvent {
  def xform(fr: Frame2d): MouseEvent = copy(pos = pos.mapValues(fr xform _))
}

/** Which physical buttons are bound to logical ones. */
object MouseBindings {
  /** The indexes of the primary/secondary mouse buttons. */
  val (firstBtn, secondBtn): (Int, Int) =
    Option(System.getProperty("flipButtons")) match {
      case Some(_) => (1, 0)
      case None    => (0, 1)
    }
}
