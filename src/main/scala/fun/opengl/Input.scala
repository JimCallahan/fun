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

import org.lwjgl.input.Keyboard
import org.lwjgl.input.Mouse

import fun.math.Index2i
import fun.math.Matrix44d
import fun.math.Vec2d

/** All input events which have occurred since the last
  * [[http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html org.lwjgl.opengl.Display.update]]
  * call.
  * @param mouse All mouse events in order.
  * @param keyboard All keyboard events in order. */
case class Input(mouse: Seq[MouseEvent], keyboard: Seq[KeyboardEvent])

object Input {
  import fun.opengl.KeyboardEvent._

  Keyboard.enableRepeatEvents(true)

  /** The state of the SHIFT key after the last keyboard event was processed. */
  private var shiftState: ShiftKeyState = ShiftUp

  /** The state of the CONTROL key after the last keyboard event was processed. */
  private var controlState: ControlKeyState = ControlUp

  /** The state of the META (ALT) key after the last keyboard event was processed. */
  private var metaState: MetaKeyState = MetaUp

  /** Provides the keyboard events since the last
    * [[http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html org.lwjgl.opengl.Display.update]]
    * call as an iterator. */
  private class KeyboardEvents extends Iterator[KeyboardEvent] {

    /** Whether the next event has been polled. */
    private var isNext = false

    def hasNext: Boolean = {
      if (isNext) true
      else {
        isNext = Keyboard.next
        isNext
      }
    }

    def next: KeyboardEvent = {
      if (!hasNext)
        throw new IllegalStateException("Should not be calling this if hasNext() is false!")

      val nanos = Keyboard.getEventNanoseconds
      val char = Keyboard.getEventCharacter
      val keyCode = Keyboard.getEventKey
      val action = if (Keyboard.getEventKeyState) Pressed else Released

      import Keyboard.{ KEY_LCONTROL, KEY_RCONTROL, KEY_LMENU, KEY_RMENU, KEY_LSHIFT, KEY_RSHIFT }
      shiftState = (keyCode, action) match {
        case (KEY_LSHIFT | KEY_RSHIFT, Pressed)  => ShiftDown
        case (KEY_LSHIFT | KEY_RSHIFT, Released) => ShiftUp
        case _                                   => shiftState
      }
      controlState = (keyCode, action) match {
        case (KEY_LCONTROL | KEY_RCONTROL, Pressed)  => ControlDown
        case (KEY_LCONTROL | KEY_RCONTROL, Released) => ControlUp
        case _                                       => controlState
      }
      metaState = (keyCode, action) match {
        case (KEY_LMENU | KEY_RMENU, Pressed)  => MetaDown // TODO - Why isn't this LMETA/RMETA?!
        case (KEY_LMENU | KEY_RMENU, Released) => MetaUp
        case _                                 => metaState
      }

      val event = KeyboardEvent(nanos, char, keyCode, action, shiftState, controlState, metaState)

      isNext = false
      event
    }
  }

  /** Provides the mouse events since the last
    * [[http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html org.lwjgl.opengl.Display.update]]
    * call as an iterator.
    * @param diplaySize The display resolution.
    * @param xforms The named viewing transformations.*/
  private class MouseEvents(displaySize: Vec2d, xforms: Map[String, Matrix44d]) extends Iterator[MouseEvent] {
    import org.lwjgl.input.Mouse

    /** Whether the next event has been polled. */
    private var isNext = false

    def hasNext: Boolean = {
      if (isNext) true
      else {
        isNext = Mouse.next
        isNext
      }
    }

    private val invsXforms =
      xforms.mapValues {
        _.inverse match {
          case Some(mx) => mx
          case _        => throw new IllegalStateException("Somehow unable to invert the camera transform!")
        }
      }

    def next: MouseEvent = {
      if (!hasNext)
        throw new IllegalStateException("Should not be calling this if hasNext() is false!")

      val nanos = Mouse.getEventNanoseconds
      val epos = Index2i(Mouse.getEventX, Mouse.getEventY).toPos2d
      val wheel = Mouse.getEventDWheel
      val ndc = ((epos - (displaySize * 0.5)) * 2.0) / displaySize
      val pos = {
        Map("Device" -> epos, "NDC" -> ndc) ++
        invsXforms.mapValues(_.xform(ndc.toVec4d.updateW(1.0)).toPos2d)
      }
      val event = Mouse.getEventButton match {
        case -1 =>
          if (Mouse.hasWheel && (wheel != 0)) MouseWheel(nanos, pos, wheel)
          else MouseMove(nanos, pos)
        case id => MouseButton(nanos, pos, id, Mouse.getEventButtonState)
      }
      isNext = false
      event
    }

    /** The current mouse position as an MouseMove event. */
    def current: MouseEvent = {
      val nanos = org.lwjgl.Sys.getTime
      val epos = Index2i(Mouse.getX, Mouse.getY).toPos2d
      val ndc = ((epos - (displaySize * 0.5)) * 2.0) / displaySize
      val pos = {
        Map("Device" -> epos, "NDC" -> ndc) ++
        invsXforms.mapValues(_.xform(ndc.toVec4d.updateW(1.0)).toPos2d)
      }
      MouseCurrent(nanos, pos)
    }
  }

  /** The input events since the last
    * [[http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html org.lwjgl.opengl.Display.update]]
    * call.
    * @param cam The viewing camera. */
  def apply(cam: ZoomCamera): Input = {
    val m = new MouseEvents(cam.res, Map("Camera" -> cam.xform))
    val me = m.toSeq ++ Seq(m.current)
    val ke = (new KeyboardEvents()).toSeq
    Input(me, ke)
  }

  /** The input events since the last
    * [[http://lwjgl.org/javadoc/org/lwjgl/opengl/Display.html org.lwjgl.opengl.Display.update]]
    * call.
    *
    * Mouse events are transformed from screen space into global space.
    * @param displaySize The display resolution.
    * @param xforms The named viewing transformations. */
  def apply(displaySize: Vec2d, xforms: Map[String, Matrix44d]): Input = {
    val m = new MouseEvents(displaySize, xforms)
    val me = m.toSeq ++ Seq(m.current)
    val ke = (new KeyboardEvents()).toSeq
    Input(me, ke)
  }
}
