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

import java.nio.ByteBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.ShortBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.GL_DOUBLE
import org.lwjgl.opengl.GL11.GL_FLOAT
import org.lwjgl.opengl.GL11.GL_UNSIGNED_BYTE
import org.lwjgl.opengl.GL11.GL_UNSIGNED_INT
import org.lwjgl.opengl.GL11.GL_UNSIGNED_SHORT
import org.lwjgl.opengl.GL15.GL_ARRAY_BUFFER
import org.lwjgl.opengl.GL15.GL_STATIC_DRAW
import org.lwjgl.opengl.GL15.glBufferData
import org.lwjgl.opengl.GL15.glBufferSubData
import org.lwjgl.opengl.OpenGLException

import GLUtil.tron

/** Abstract base class for all Vertex Buffer Objects.
  * @constructor Create a new Vertex Buffer Object.
  * @param dataType The OpenGL storage type of the buffer.
  * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4.
  * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
  * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
  * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
abstract class VertexBufferObject(val dataType: Int, val attrSize: Int, val usage: Int)
    extends BufferObject(GL_ARRAY_BUFFER, "VBO") {
  if ((attrSize < 1) || (attrSize > 4))
    throw new OpenGLException("VBO attribute size (" + attrSize + ") was outside 1-4 range!")
}

/** Factory for ByteVBO. */
object ByteVBO {
  /** A loader that does nothing. */
  val Unspecified: (ByteBuffer) => Unit = { _ => }

  /** Create a VBO to hold Byte data.
    * @param numVertices The number of vertices.
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def apply(numVertices: Int, attrSize: Int, usage: Int = GL_STATIC_DRAW)(loader: ByteBuffer => Unit): ByteVBO = {
    val buf = BufferUtils.createByteBuffer(numVertices * attrSize)
    val vbo = new ByteVBO(attrSize, usage, buf)
    loader(buf)
    buf.rewind
    vbo.load
    buf.clear
    vbo
  }
}

/** Vertex Buffer Object containing Byte attribute data. */
class ByteVBO private (attrSize: Int, usage: Int, buf: ByteBuffer)
    extends VertexBufferObject(GL_UNSIGNED_BYTE, attrSize, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(): Unit = {
    state match {
      case Reserved => {
        bind
        tron { glBufferData(target, buf, usage) }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def reload(offset: Long)(loader: ByteBuffer => Unit): Unit = {
    loader(buf)
    buf.rewind
    state match {
      case Loaded => {
        bind
        tron { glBufferSubData(target, offset, buf) }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
    buf.clear
  }
}

/** Factory for ShortVBO. */
object ShortVBO {
  /** A loader that does nothing. */
  val Unspecified: (ShortBuffer) => Unit = { _ => }

  /** Create a VBO to hold Short data.
    * @param numVertices The number of vertices.
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def apply(numVertices: Int, attrSize: Int, usage: Int = GL_STATIC_DRAW)(loader: ShortBuffer => Unit): ShortVBO = {
    val buf = BufferUtils.createShortBuffer(numVertices * attrSize)
    val vbo = new ShortVBO(attrSize, usage, buf)
    loader(buf)
    buf.rewind
    vbo.load
    buf.clear
    vbo
  }
}

/** Vertex Buffer Object containing Short attribute data. */
class ShortVBO private (attrSize: Int, usage: Int, buf: ShortBuffer)
    extends VertexBufferObject(GL_UNSIGNED_SHORT, attrSize, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(): Unit = {
    state match {
      case Reserved => {
        bind
        tron { glBufferData(target, buf, usage) }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def reload(offset: Long)(loader: ShortBuffer => Unit): Unit = {
    loader(buf)
    buf.rewind
    state match {
      case Loaded => {
        bind
        tron { glBufferSubData(target, offset, buf) }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
    buf.clear
  }
}

/** Factory for IntVBO. */
object IntVBO {
  /** A loader that does nothing. */
  val Unspecified: (IntBuffer) => Unit = { _ => }

  /** Create a VBO to hold Int data.
    * @param numVertices The number of vertices.
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def apply(numVertices: Int, attrSize: Int, usage: Int = GL_STATIC_DRAW)(loader: IntBuffer => Unit): IntVBO = {
    val buf = BufferUtils.createIntBuffer(numVertices * attrSize)
    val vbo = new IntVBO(attrSize, usage, buf)
    loader(buf)
    buf.rewind
    vbo.load
    buf.clear
    vbo
  }
}

/** Vertex Buffer Object containing Int attribute data. */
class IntVBO private (attrSize: Int, usage: Int, buf: IntBuffer)
    extends VertexBufferObject(GL_UNSIGNED_INT, attrSize, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(): Unit = {
    state match {
      case Reserved => {
        bind
        tron { glBufferData(target, buf, usage) }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def reload(offset: Long)(loader: IntBuffer => Unit): Unit = {
    loader(buf)
    buf.rewind
    state match {
      case Loaded => {
        bind
        tron { glBufferSubData(target, offset, buf) }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
    buf.clear
  }
}

/** Factory for FloatVBO. */
object FloatVBO {
  /** A loader that does nothing. */
  val Unspecified: (FloatBuffer) => Unit = { _ => }

  /** Create a VBO to hold Float data.
    * @param numVertices The number of vertices.
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def apply(numVertices: Int, attrSize: Int, usage: Int = GL_STATIC_DRAW)(loader: FloatBuffer => Unit): FloatVBO = {
    val buf = BufferUtils.createFloatBuffer(numVertices * attrSize)
    val vbo = new FloatVBO(attrSize, usage, buf)
    loader(buf)
    buf.rewind
    vbo.load
    buf.clear
    vbo
  }
}

/** Vertex Buffer Object containing Float attribute data. */
class FloatVBO private (attrSize: Int, usage: Int, buf: FloatBuffer)
    extends VertexBufferObject(GL_FLOAT, attrSize, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(): Unit = {
    state match {
      case Reserved => {
        bind
        tron { glBufferData(target, buf, usage) }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def reload(offset: Long)(loader: FloatBuffer => Unit): Unit = {
    loader(buf)
    buf.rewind
    state match {
      case Loaded => {
        bind
        tron { glBufferSubData(target, offset, buf) }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
    buf.clear
  }
}

/** Factory for DoubleVBO. */
object DoubleVBO {
  /** A loader that does nothing. */
  val Unspecified: (DoubleBuffer) => Unit = { _ => }

  /** Create a VBO to hold Double data.
    * @param numVertices The number of vertices.
    * @param attrSize The number of attribute values per vertex: 1, 2, 3 or 4
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def apply(numVertices: Int, attrSize: Int, usage: Int = GL_STATIC_DRAW)(loader: DoubleBuffer => Unit): DoubleVBO = {
    val buf = BufferUtils.createDoubleBuffer(numVertices * attrSize)
    val vbo = new DoubleVBO(attrSize, usage, buf)
    loader(buf)
    buf.rewind
    vbo.load
    buf.clear
    vbo
  }
}

/** Vertex Buffer Object containing Double attribute data. */
class DoubleVBO private (attrSize: Int, usage: Int, buf: DoubleBuffer)
    extends VertexBufferObject(GL_DOUBLE, attrSize, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(): Unit = {
    state match {
      case Reserved => {
        bind
        tron { glBufferData(target, buf, usage) }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads vertex data into the supplied buffer. */
  def reload(offset: Long)(loader: DoubleBuffer => Unit): Unit = {
    loader(buf)
    buf.rewind
    state match {
      case Loaded => {
        bind
        tron { glBufferSubData(target, offset, buf) }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
    buf.clear
  }
}
