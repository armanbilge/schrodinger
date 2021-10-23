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

import cats.Defer
import cats.Monad
import cats.syntax.all.*
import schrodinger.kernel.Gamma
import schrodinger.kernel.Gaussian
import schrodinger.kernel.GenGaussian
import schrodinger.kernel.Uniform
import schrodinger.math.Interval.*

object gamma extends GammaInstances

trait GammaInstances:

  given schrodingerRandomGammaForDouble[
      F[_]: Monad: Defer: Uniform[0.0 <=@< 1.0, Double]: GenGaussian[0, 1, Double]]: Gamma[Double][F] =
    case Gamma.Params(shape, rate) =>
      val U = Uniform(0.0 <=@< 1.0)
      val G = Gaussian.standard
      val scale = 1 / rate

      lazy val ahrensDieter: F[Double] =
        val oneOverAlpha = 1 / shape
        val bGSOptim = 1 + shape / math.E

        lazy val step1: F[Double] = U.flatMap { u =>
          val p = bGSOptim * u
          if p <= 1 then step2(p)
          else step3(p)
        }

        def step2(p: Double): F[Double] =
          val x = math.pow(p, oneOverAlpha)
          U.flatMap { u2 =>
            if u2 > math.exp(-x) then step1
            else (scale * x).pure
          }

        def step3(p: Double): F[Double] =
          val x = -math.log((bGSOptim - p) * oneOverAlpha)
          U.flatMap { u2 =>
            if u2 <= math.pow(x, shape - 1) then (scale * x).pure
            else step1
          }

        step1

      lazy val marsagliaTsang: F[Double] =
        val dOptim = shape - 1 / 3.0
        val cOptim = (1 / 3.0) / math.sqrt(dOptim)

        G.flatMap { x =>
          val oPcTx = 1 + cOptim * x
          val v = oPcTx * oPcTx * oPcTx

          if v <= 0 then marsagliaTsang
          else
            val x2 = x * x
            U.flatMap { u =>
              if u < 1 - 0.0331 * x2 * x2 then (scale * dOptim * v).pure
              else if math.log(u) < 0.5 * x2 + dOptim * (1 - v + math.log(v)) then
                (scale * dOptim * v).pure
              else marsagliaTsang
            }
        }

      if shape < 1 then ahrensDieter else marsagliaTsang
