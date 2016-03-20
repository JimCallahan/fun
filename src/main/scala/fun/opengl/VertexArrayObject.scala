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

import org.lwjgl.opengl.GL20.glEnableVertexAttribArray
import org.lwjgl.opengl.GL20.glVertexAttribPointer
import org.lwjgl.opengl.GL30.glBindVertexArray
import org.lwjgl.opengl.GL30.glDeleteVertexArrays
import org.lwjgl.opengl.GL30.glGenVertexArrays

import GLUtil.tron

/** Factory for VertexArrayObject */
object VertexArrayObject {
  /** Create a new VAO. */
  def apply(vbos: List[VertexBufferObject], ibo: IndexBufferObject): VertexArrayObject =
    new VertexArrayObject(vbos, ibo)
}

/** Vertex Array Object. */
class VertexArrayObject(val vbos: List[VertexBufferObject], val ibo: IndexBufferObject)
    extends BaseObject("VAO") {
  /** The unique OpenGL handle for the VAO. */
  protected val objectID = {
    val id = tron { glGenVertexArrays }
    state = Reserved
    id
  }

  // Initialize the VAO.
  {
    bind
    for ((vbo, idx) <- vbos.zip(0 until vbos.length)) {
      vbo.bind
      tron { glEnableVertexAttribArray(idx) }
      tron { glVertexAttribPointer(idx, vbo.attrSize, vbo.dataType, false, 0, 0) }
    }
    ibo.bind
    unbind

    state = Loaded
  }

  /** Bind the VAO to the current context. */
  protected def bindGL(): Unit =
    tron { glBindVertexArray(objectID) }

  /** Unbind the VAO from the current context. */
  protected def unbindGL(): Unit =
    tron { glBindVertexArray(0) }

  /** Release the OpenGL resources for the VAO. */
  protected def releaseGL(): Unit =
    tron { glDeleteVertexArrays(objectID) }
}
