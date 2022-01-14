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
import schrodinger.kernel.{Exponential, GenExponential, Uniform}
import schrodinger.math.Interval.*
import schrodinger.math.Interval.given

object exponential extends ExponentialInstances

trait ExponentialInstances:

  given schrodingerRandomExponentialForDouble[F[_]: Functor: GenExponential[1, Double]]
      : Exponential[Double][F] = params =>
    import params.*
    Exponential.standard.map(_ / rate)

  given schrodingerRandomStandardExponentialForDouble[
      F[_]: Monad: Uniform[0.0 <=@< 1.0, Double]: Uniform[0.0 <@<= 1.0, Double]]
      : GenExponential[1, Double][F] =
    val standard: F[Double] =
      import AhrensDieterConstants.*
      val umin =
        Vector.iterate(Uniform(0.0 <=@< 1.0), 16)(_.map2(Uniform(0.0 <=@< 1.0))(Math.min))
      Uniform(0.0 <@<= 1.0).flatMap { _u =>
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

    _ => standard

  private object AhrensDieterConstants:
    val ExponentialSaQi =
      val qi = new Array[Double](16)
      val log2 = Math.log(2)
      var p = log2
      qi(0) = p
      var i = 1
      while i < qi.length do
        p *= log2 / (i + 1)
        qi(i) = qi(i - 1) + p
        i += 1
      qi
