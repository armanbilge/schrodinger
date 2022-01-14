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

import cats.Monad
import cats.syntax.all.*
import schrodinger.kernel.Beta
import schrodinger.kernel.Uniform
import schrodinger.math.Interval.*

object beta extends BetaInstances

trait BetaInstances:

  given schrodingerRandomBetaForDouble[F[_]: Monad: Uniform[0.0 <=@< 1.0, Double]]
      : Beta[Double][F] with
    private val Ln4 = Math.log(4)
    private val Ln5p1 = 1 + Math.log(5)

    def apply(params: Beta.Params[Double, Double]): F[Double] =
      val U = Uniform(0.0 <=@< 1.0)

      val a = Math.min(params.alpha, params.beta)
      val b = Math.max(params.alpha, params.beta)
      val aIsAlphaShape = a == params.alpha

      val alpha = a + b
      val logAlpha = Math.log(alpha)

      def computeX(aIsAlphaShape: Boolean, b: Double)(w: Double): Double =
        val tmp = Math.min(w, Double.MaxValue)
        if aIsAlphaShape then tmp / (b + tmp) else b / (b + tmp)

      def bb(aIsAlphaShape: Boolean, a: Double, b: Double): F[Double] =
        val beta = Math.sqrt((alpha - 2) / (2 * a * b - alpha))
        val gamma = a + 1 / beta

        val W = (U, U).mapN { (u1, u2) =>
          val v = beta * (Math.log(u1) - Math.log1p(-u1))
          val w = a * Math.exp(v)
          val z = u1 * u1 * u2
          val r = gamma * v - Ln4
          val s = a + r - w

          if s + Ln5p1 >= 5 * z then Some(w)
          else
            val t = Math.log(z)
            if s >= t then Some(w)
            else if r + alpha * (logAlpha - Math.log(b + w)) < t then None
            else Some(w)
        }

        W.untilDefinedM.map(computeX(aIsAlphaShape, b))

      def bc(aIsAlphaShape: Boolean, a: Double, b: Double): F[Double] =
        val beta = 1 / b
        val delta = 1 + a - b
        val k1 = delta * (1.0 / 72.0 + 3.0 / 72.0 * b) / (a * beta - 7.0 / 9.0)
        val k2 = 0.25 + (0.5 + 0.25 / delta) * b

        val W = (U, U).mapN { (u1, u2) =>
          val y = u1 * u2
          val z = u1 * y

          inline def step5 =
            val v = beta * (Math.log(u1) - Math.log1p(-u1))
            val w = a * Math.exp(v)
            if alpha * (logAlpha - Math.log(b + w) + v) - Ln4 >= Math.log(z) then Some(w)
            else None

          if u1 < 0.5 then
            if 0.25 * u2 + z - y >= k1 then None
            else step5
          else if z <= 0.25 then
            val v = beta * (Math.log(u1) - Math.log1p(-u1))
            val w = a * Math.exp(v)
            Some(w)
          else if z >= k2 then None
          else step5

        }

        W.untilDefinedM.map(computeX(aIsAlphaShape, b))

      if a > 1 then bb(aIsAlphaShape, a, b)
      else bc(!aIsAlphaShape, b, a)
