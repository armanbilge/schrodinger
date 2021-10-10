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

package schrodinger.random

import cats.Functor
import cats.Monad
import cats.syntax.all.given
import schrodinger.kernel.{Gaussian, GenGaussian, Uniform}
import schrodinger.math.Interval.*
import schrodinger.math.Interval.given

object gaussian extends GaussianInstances

trait GaussianCache[F[_], A]:
  def get: F[A]
  def set(a: A): F[Unit]

trait GaussianInstances:
  given schrodingerRandomGaussianForDouble[F[_]: Functor: GenGaussian[0, 1, Double]]
      : GenGaussian[0, 1, Double][F] =
    params =>
      import params.*
      Gaussian.standard.map(_ * standardDeviation + mean)

  given schrodingerRandomStandardGaussianForDouble[F[_]: Monad: Uniform[0.0 <=@< 1.0, Double]](
      using cache: GaussianCache[F, Double]): GenGaussian[0, 1, Double][F] =

    final case class BoxMuller(x: Double, y: Double, s: Double)
    object BoxMuller:
      def apply(x: Double, y: Double): BoxMuller =
        BoxMuller(x, y, x * x + y * y)

    val boxMuller =
      Uniform(0.0 <=@< 1.0).map2(Uniform(0.0 <=@< 1.0)) { (x, y) =>
        BoxMuller(x * 2 - 1, y * 2 - 1)
      }

    val pair = boxMuller.iterateWhile(bm => bm.s >= 1.0 | bm.s == 0.0).map { bm =>
      import bm.*
      val scale = math.sqrt(-2.0 * math.log(s) / s)
      (scale * x, scale * y)
    }

    val standard = cache.get.flatMap { cached =>
      if cached.isNaN then pair.flatMap { (x, y) => cache.set(y).as(x) }
      else cache.set(Double.NaN).as(cached)
    }

    _ => standard
