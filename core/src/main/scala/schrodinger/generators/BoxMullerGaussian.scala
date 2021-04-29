/*
 * Copyright 2021 Arman Bilge
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

package schrodinger.generators

import cats.Id
import cats.data.StateT
import cats.syntax.monad._
import schrodinger.generators.BoxMullerGaussian.BoxMuller

/**
 * Implements the Box-Muller algorithm for generating Gaussians.
 */
trait BoxMullerGaussian[S] extends Generator[S] {

  protected def getExtraGaussian: StateT[Id, S, Double]

  protected def setExtraGaussian(x: Double): StateT[Id, S, Unit]

  override def nextGaussian: StateT[Id, S, Double] =
    getExtraGaussian.flatMap { x =>
      if (!x.isNaN)
        setExtraGaussian(Double.NaN).map(_ => x)
      else
        for {
          bm <- nextBoxMuller.iterateWhile(bm => bm.s >= 1.0 | bm.s == 0.0)
          scale = math.sqrt(-2.0 * math.log(bm.s) / bm.s)
          _ <- setExtraGaussian(bm.y * scale)
        } yield bm.x * scale
    }

  private def nextBoxMuller: StateT[Id, S, BoxMuller] = {
    val nextDouble = this.nextDouble
    for {
      x <- nextDouble
      y <- nextDouble
    } yield BoxMuller(x * 2 - 1, y * 2 - 1)
  }
}

object BoxMullerGaussian {

  final private case class BoxMuller(x: Double, y: Double, s: Double)

  private object BoxMuller {
    def apply(x: Double, y: Double): BoxMuller =
      BoxMuller(x, y, x * x + y * y)
  }
}
