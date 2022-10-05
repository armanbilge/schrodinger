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

package schrodinger.kernel

import algebra.ring.Rig
import cats.Monad
import cats.data.OptionT
import cats.syntax.all.*

trait Gaussian[F[_], A]:
  def gaussian: F[A]
  def gaussian(mean: A, standardDeviation: A): F[A]

trait GaussianCache[F[_], A]:
  def getAndClear: F[Option[A]]
  def set(a: A): F[Unit]

object Gaussian:
  inline def apply[F[_], A](mean: A, standardDeviation: A)(using g: Gaussian[F, A]): F[A] =
    g.gaussian(mean, standardDeviation)

  inline def standard[F[_], A](using g: Gaussian[F, A]): F[A] = g.gaussian

  given [F[_]: Monad](using
      U: Uniform[F, Double],
      cache: GaussianCache[F, Double],
  ): Gaussian[F, Double] with
    def gaussian(mean: Double, standardDeviation: Double) =
      gaussian.map(_ * standardDeviation + mean)

    def gaussian =
      val sampleAndCache = for
        x <- U.uniform01
        y <- U.uniform10
        alpha = 2 * Math.PI * x
        r = Math.sqrt(-2 * Math.log(y))
        g1 = r * Math.cos(alpha)
        g2 = r * Math.sin(alpha)
        _ <- cache.set(g2)
      yield g1

      OptionT(cache.getAndClear).getOrElseF(sampleAndCache)
