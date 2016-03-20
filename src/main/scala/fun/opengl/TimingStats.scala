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

import org.lwjgl.Sys

import fun.util.Logging

/** Factory for TimingStats. */
object TimingStats {
  /** Create new timing statistics counters. */
  def apply(): TimingStats = new TimingStats()
}

/** Time statistics. */
class TimingStats
    extends Logging {

  /** The timestamp of the start of the frame. */
  private var stamp = Sys.getTime

  /** The timestamp of when FPS was last calculated. */
  private var lastFPS = Sys.getTime

  /** The number of frames rendered since the last FPS rate was calculated. */
  private var frameCount = 0

  /** The amount of FPS for each second the viewer was running. */
  private var fpsHistory: List[Int] = Nil

  /** Update all timing statistics.
    *
    * @return The current timestamp.
    */
  def update: Long = {
    val stamp = Sys.getTime
    val sinceFPS = stamp - lastFPS
    if (sinceFPS > Sys.getTimerResolution) {
      fpsHistory = frameCount :: fpsHistory
      frameCount = 0
      lastFPS = stamp
    }
    frameCount = frameCount + 1
    stamp
  }

  /** Report timing statistics. */
  def report() {
    val avgFPS = ((0L /: fpsHistory)(_ + _)).toDouble / fpsHistory.length.toDouble
    log.info(f"Average FPS: $avgFPS%.2f")
    log.info("FPS History: " + fpsHistory.map(_.toString).mkString(" "))
  }
}
