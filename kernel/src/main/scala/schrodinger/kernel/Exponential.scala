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

import cats.Monad
import cats.syntax.all.*

trait Exponential[F[_], A] {
  def exponential: F[A]
  def exponential(rate: A): F[A]
}

object Exponential {
  inline def apply[F[_], A](rate: A)(using e: Exponential[F, A]): F[A] =
    e.exponential(rate)

  inline def standard[F[_], A](using e: Exponential[F, A]): F[A] =
    e.exponential

  given [F[_]: Monad](using U: Uniform[F, Double]): Exponential[F, Double] with {
    def exponential(rate: Double) = exponential.map(_ / rate)

    def exponential = U.uniform10.flatMap { _u =>
      var a = 0.0
      var u = _u

      while u < 0.5 do {
        a += ExponentialSaQi(0)
        u *= 2
      }

      u += u - 1

      if u <= ExponentialSaQi(0) then (a + u).pure
      else {
        var i = 1
        while u > ExponentialSaQi(i) do i += 1

        umin(i).map(a + _ * ExponentialSaQi(0))
      }
    }

    private val umin =
      Vector.iterate(Uniform.standard, 16)(_.map2(Uniform.standard)(Math.min))

    private val ExponentialSaQi = {
      val qi = new Array[Double](16)
      val log2 = Math.log(2)
      var p = log2
      qi(0) = p
      var i = 1
      while i < qi.length do {
        p *= log2 / (i + 1)
        qi(i) = qi(i - 1) + p
        i += 1
      }
      qi
    }
  }
}
