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

import java.awt.image.BufferedImage
import java.nio.file.Path

import javax.imageio.ImageIO

import scala.util.Sorting

import fun.math.Index2i
import fun.math.Pos2d
import fun.math.Scalar
import fun.math.Space2d

object VectorTexture {
  /** Encode a high resolution black/white image as a distance vector texture.
    * @param inPath The input black/white image.
    * @param outPath The output distance vector texture.
    * @param resolution The output texture resolution (square image).
    * @param maxDistance The maximum distance (in coordinate space) to encode.
    */
  def encode(inPath: Path, outPath: Path, resolution: Int, maxDistance: Double): Unit = {
    val inImage = ImageIO.read(inPath.toFile)
    val inRaster = inImage.getRaster
    val inSpace = {
      val b = inRaster.getBounds
      Space2d(Index2i(b.width, b.height), Pos2d(b.x, b.y), Pos2d(b.x + b.width, b.y + b.height))
    }

    val outImage = new BufferedImage(resolution, resolution, BufferedImage.TYPE_BYTE_GRAY)
    val outRaster = outImage.getRaster
    val outSpace = Space2d(Index2i(resolution), Pos2d(0.0), Pos2d(1.0))

    val indexSpan =
      inSpace.indexOf(inSpace.position(Pos2d(maxDistance))) -
    inSpace.indexOf(inSpace.position(Pos2d(0.0)))

    val kernel = {
      val quads = List(Index2i(-1, 1), Index2i(-1, -1), Index2i(1, -1))
      val zeroCoord = inSpace.cellCoords(Index2i(0)).center
      val idxDist =
        for {
          ix <- 0 until indexSpan.x
          iy <- 0 until indexSpan.y
          idx = Index2i(ix, iy)
          dist = (inSpace.cellCoords(idx).center - zeroCoord).mag
          if (dist < maxDistance)
            } yield {
          ((ix, iy) match {
            case (0, 0) => Nil
            case (0, _) => List((idx, dist), (idx * Index2i(0, -1), dist))
            case (_, 0) => List((idx, dist), (idx * Index2i(-1, 0), dist))
            case _ =>
              val qs =
                if (ix == iy) quads
                else quads ++ quads.map { i => Index2i(i.y, i.x) }
                  (idx, dist) :: qs.map { q => (idx * q, dist) }
          })
        }

      val ary = idxDist.flatten.toArray
      Sorting.quickSort(ary)(Ordering.by[(Index2i, Double), Double](_._2))
      ary.toList
    }

    for {
      ix <- 0 until outSpace.size.x
      iy <- 0 until outSpace.size.y
    } {
      val coord = outSpace.cellCoords(Index2i(ix, iy)).center
      val idx = inSpace.indexOf(inSpace.position(coord))
      val sample = inRaster.getSampleDouble(idx.x, idx.y, 0) / 255.0
      val on = sample > 0.5

      def distance(kern: List[(Index2i, Double)], cnt: Int): (Double, Int) = {
        kern match {
          case Nil => (maxDistance, cnt)
          case (ki, dist) :: ks =>
            val sidx = idx + ki
            val sd = if (sidx.forany(Index2i(0))(_ < _) || sidx.forany(inSpace.size)(_ >= _)) None
            else {
              val ssample = inRaster.getSampleDouble(sidx.x, sidx.y, 0) / 255.0
              val son = ssample > 0.5
              if ((on && !son) || (!on && son)) Some(dist) else None
            }

            sd match {
              case Some(d) => (d, cnt)
              case None    => distance(ks, cnt + 1)
            }
        }
      }

      val (minDist, count) = distance(kernel, 0)
      val dist = Scalar.clamp(minDist, 0.0, maxDistance) / (maxDistance * 2.0)
      val norm = if (on) 0.5 + dist else 0.5 - dist

      outRaster.setSample(ix, iy, 0, norm * 255.0)
    }

    ImageIO.write(outImage, "png", outPath.toFile)
  }
}
