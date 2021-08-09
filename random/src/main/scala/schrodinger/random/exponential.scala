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
import cats.syntax.all._
import schrodinger.kernel.{Exponential, Uniform}

object exponential extends ExponentialInstances

trait ExponentialInstances {

  implicit def schrodingerRandomExponentialForDouble[F[_]: Monad: Uniform[*[_], Unit, Double]]
      : Exponential[F, Double] =
    new Exponential[F, Double] {
      override def apply(rate: Double): F[Double] =
        standard.map(_ / rate)

      override val standard: F[Double] = {
        import AhrensDieterConstants._
        val umin = Vector.iterate(Uniform.standard, 16)(_.map2(Uniform.standard)(math.min))
        Uniform.standard.flatMap { _u =>
          var a = 0.0
          var u = _u

          while (u < 0.5) {
            a += ExponentialSaQi(0)
            u *= 2
          }

          u += u - 1

          if (u <= ExponentialSaQi(0))
            (a + u).pure
          else {
            var i = 1
            while (u > ExponentialSaQi(i)) i += 1

            umin(i).map(a + _ * ExponentialSaQi(0))
          }
        }
      }
    }

  private object AhrensDieterConstants {
    val ExponentialSaQi = {
      val qi = new Array[Double](16)
      val log2 = math.log(2)
      var p = log2
      qi(0) = p
      var i = 1
      while (i < qi.length) {
        p *= log2 / (i + 1)
        qi(i) = qi(i - 1) + p
        i += 1
      }
      qi
    }
  }
}