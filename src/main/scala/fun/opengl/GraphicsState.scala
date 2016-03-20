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

trait GraphicsState {
  /** Render the current graphics state while generating the state for the next frame. */
  def render: GraphicsState

  /** Whether the application should continue rendering (true) or exit (false). */
  def isAlive: Boolean = true

  /** Run during normal application exit. */
  def onExit(): Unit = {}
}
