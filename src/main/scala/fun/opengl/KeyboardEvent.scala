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

object KeyboardEvent {
  sealed trait KeyAction
  case object Pressed extends KeyAction
  case object Released extends KeyAction

  sealed trait ShiftKeyState
  case object ShiftDown extends ShiftKeyState
  case object ShiftUp extends ShiftKeyState

  sealed trait ControlKeyState
  case object ControlDown extends ControlKeyState
  case object ControlUp extends ControlKeyState

  sealed trait MetaKeyState
  case object MetaDown extends MetaKeyState
  case object MetaUp extends MetaKeyState
}

/** An event involving a change in keyboard state.
  * @param nanos The time in nanoseconds of the current event.
  * @param character The character involved.
  * @param keyCode The org.lwjgl.input.Keyboard._ code for the key involved.
  * @param action The change of key state that caused the event.
  * @param shift The state of the SHIFT key during the event.
  * @param control The state of the CONTROL key during the event.
  * @param meta The state of the META key during the event.
  */
case class KeyboardEvent(
  nanos: Long,
  character: Char,
  keyCode: Int,
  action: KeyboardEvent.KeyAction,
  shift: KeyboardEvent.ShiftKeyState,
  control: KeyboardEvent.ControlKeyState,
  meta: KeyboardEvent.MetaKeyState
) {
  import KeyboardEvent._

  /** The ASCII code for the key involved. */
  lazy val asciiCode: Int = character.toInt & 0xffff

  /** Is the key in the ASCII Printable range (32-126). */
  def isPrintable: Boolean = (asciiCode >= 32) && (asciiCode <= 126)

  /** Whether the action causing the event was a key press (true) or release (false). */
  def isPressed: Boolean =
    action match {
      case Pressed  => true
      case Released => false
    }

  /** Whether the SHIFT key was depressed during the event. */
  def isShift: Boolean =
    shift match {
      case ShiftDown => true
      case ShiftUp   => false
    }

  /** Whether the CONTROL key was depressed during the event. */
  def isControl: Boolean =
    control match {
      case ControlDown => true
      case ControlUp   => false
    }

  /** Whether the META key was depressed during the event. */
  def isMeta: Boolean =
    meta match {
      case MetaDown => true
      case MetaUp   => false
    }
}
