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
import cats.syntax.all.given
import schrodinger.kernel.{Exponential, Uniform}

object exponential extends ExponentialInstances

trait ExponentialInstances:

  given schrodingerRandomExponentialForDouble[F[_]: Monad: Uniform[Unit, Double]]
      : Exponential[Double][F] with
    override def apply(params: Exponential.Params[Double]): F[Double] =
      import params.*
      standard.map(_ / rate)

    private val standard: F[Double] =
      import AhrensDieterConstants.*
      val umin = Vector.iterate(Uniform.standard, 16)(_.map2(Uniform.standard)(math.min))
      Uniform.standard.flatMap { _u =>
        var a = 0.0
        var u = _u

        while u < 0.5 do
          a += ExponentialSaQi(0)
          u *= 2

        u += u - 1

        if u <= ExponentialSaQi(0) then (a + u).pure
        else
          var i = 1
          while u > ExponentialSaQi(i) do i += 1

          umin(i).map(a + _ * ExponentialSaQi(0))
      }

  private object AhrensDieterConstants:
    val ExponentialSaQi =
      val qi = new Array[Double](16)
      val log2 = math.log(2)
      var p = log2
      qi(0) = p
      var i = 1
      while i < qi.length do
        p *= log2 / (i + 1)
        qi(i) = qi(i - 1) + p
        i += 1
      qi
