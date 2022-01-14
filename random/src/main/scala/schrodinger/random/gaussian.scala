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
      : Gaussian[Double][F] =
    params =>
      import params.*
      Gaussian.standard.map(_ * standardDeviation + mean)

  given schrodingerRandomStandardGaussianForDouble[
      F[_]: Monad: Uniform[0.0 <=@< 1.0, Double]: Uniform[0.0 <@<= 1.0, Double]](
      using cache: GaussianCache[F, Double]): GenGaussian[0, 1, Double][F] =

    val sampleAndCache = for
      x <- Uniform(0.0 <=@< 1.0)
      y <- Uniform(0.0 <@<= 1.0)
      alpha = 2 * Math.PI * x
      r = Math.sqrt(-2 * Math.log(y))
      g1 = r * Math.cos(alpha)
      g2 = r * Math.sin(alpha)
      _ <- cache.set(g2)
    yield g1

    val standard = for
      cached <- cache.get
      sample <-
        if cached.isNaN then sampleAndCache
        else cache.set(Double.NaN).as(cached)
    yield sample

    _ => standard
