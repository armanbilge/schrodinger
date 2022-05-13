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

import algebra.ring.MultiplicativeMonoid
import cats.Monad
import cats.syntax.all.*

trait Gamma[F[_], A]:
  def gamma(shape: A, rate: A): F[A]

object Gamma:
  inline def apply[F[_], A](shape: A, rate: A)(using g: Gamma[F, A]): F[A] =
    g.gamma(shape, rate)

  given [F[_]: Monad](using Uniform[F, Double], Gaussian[F, Double]): Gamma[F, Double] with
    def gamma(shape: Double, rate: Double) =
      val U = Uniform.standard
      val G = Gaussian.standard
      val scale = 1 / rate
      val none = Option.empty[Double].pure

      def ahrensDieter: F[Double] =
        val oneOverAlpha = 1 / shape
        val bGSOptim = 1 + shape / Math.E

        val go: F[Option[Double]] = U.flatMap { u =>
          val p = bGSOptim * u
          if p <= 1 then
            val x = Math.pow(p, oneOverAlpha)
            U.flatMap { u2 =>
              if u2 > Math.exp(-x) then none
              else (scale * x).some.pure
            }
          else
            val x = -Math.log((bGSOptim - p) * oneOverAlpha)
            U.flatMap { u2 =>
              if u2 <= Math.pow(x, shape - 1) then (scale * x).some.pure
              else none
            }
        }

        go.untilDefinedM

      def marsagliaTsang: F[Double] =
        val dOptim = shape - 1 / 3.0
        val cOptim = 1 / 3.0 / Math.sqrt(dOptim)

        val go: F[Option[Double]] = G.flatMap { x =>
          val oPcTx = 1 + cOptim * x
          val v = oPcTx * oPcTx * oPcTx

          if v <= 0 then none
          else
            val x2 = x * x
            U.flatMap { u =>
              if u < 1 - 0.0331 * x2 * x2 then Some(scale * dOptim * v).pure
              else if Math.log(u) < 0.5 * x2 + dOptim * (1 - v + Math.log(v)) then
                Some(scale * dOptim * v).pure
              else none
            }
        }

        go.untilDefinedM

      if shape < 1 then ahrensDieter else marsagliaTsang
