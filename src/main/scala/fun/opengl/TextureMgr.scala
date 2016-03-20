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

import scala.collection.immutable.Map
import scala.collection.mutable.HashMap

import org.lwjgl.opengl.GL11.glGetInteger
import org.lwjgl.opengl.GL20.GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS

import GLUtil.tron

/** A central manager of textures, samplers and their bindings to texture units. */
object TextureMgr {
  import scala.collection.mutable.HashMap

  /** A table of named texture/sampler pairs. */
  private val managed = HashMap[String, (TextureObject, SamplerObject)]()

  /** The names of the texture/sampler pair currently bound to each texture unit, if any. */
  private val bound: Array[Option[String]] = {
    val maxUnits = tron { glGetInteger(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS) }
    Array.tabulate(maxUnits) {
      n => if (n == 0) Some("BINDING") else None
    }
  }

  /** Add a named texture/sampler pair to those currently managed,
    * replacing and unbinding any previous texture/sampler with the same name. */
  def add(name: String, texture: TextureObject, sampler: SamplerObject): Unit = {
    if (managed.contains(name)) remove(name)
    managed += (name -> ((texture, sampler)))
  }

  /** Remove a named texture/sampler pair from those currently managed,
    * unbinding them if currently bound. */
  def remove(name: String): Unit = {
    val (texture, sampler) = managed.get(name) match {
      case Some(ts) => ts
      case _ => throw new IllegalArgumentException(
        "The texture/sampler named (" + name + ") is not currently being managed!")
    }
    managed -= name

    for ((sn, tu) <- bound.zipWithIndex) {
      sn match {
        case Some(n) if (n == name) => {
          texture.unbind(tu)
          sampler.unbind(tu)
          bound(tu) = None
        }
        case _ =>
      }
    }
  }

  /** Get the texture/sampler pair associated with the given name (if any). */
  def get(name: String): Option[(TextureObject, SamplerObject)] = managed.get(name)

  /** Whether texture/sampler pair of the given name exists. */
  def contains(name: String): Boolean = managed.contains(name)

  /** Bind the given named texture/sampler pairs to available texture units while preserving
    * bindings for the given pairs that already exist and unbinding any not included.
    * @return The resulting texture/sampler to texture unit bindings. */
  def bind(names: Set[String]): Map[String, Int] = {
    for ((sn, tu) <- bound.zipWithIndex) {
      sn match {
        case Some("BINDING") =>
        case Some(n) if (!names.contains(n)) =>
          managed.get(n) match {
            case Some((texture, sampler)) =>
              texture.unbind(tu)
              sampler.unbind(tu)
            case _ => throw new IllegalArgumentException(
              "The texture/sampler named (" + n + ") is not currently being managed!")
          }
          bound(tu) = None
        case _ =>
      }
    }

    for (n <- names.filter(nn => !bound.contains(Some(nn)))) {
      bound.indexWhere(!_.isDefined) match {
        case -1 => throw new IllegalStateException(
          "There are not enough texture units available to bind all of the given texture/samplers!")
        case tu =>
          managed.get(n) match {
            case Some((texture, sampler)) =>
              texture.bind(tu)
              sampler.bind(tu)
            case _ => throw new IllegalArgumentException(
              "The texture/sampler named (" + n + ") is not currently being managed!")
          }
          bound(tu) = Some(n)
      }
    }

    bindings
  }

  /** Get the names of the current texture/sampler pairs bound to texture units. */
  def bindings: Map[String, Int] = {
    val ary =
      for {
        (nopt, i) <- bound.zipWithIndex
        n <- nopt
      } yield (n, i)
    Map.empty ++ ary
  }
}
