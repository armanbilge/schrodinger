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

trait Gamma[F[_], A] extends Exponential[F, A]:
  def gamma(shape: A, rate: A): F[A]

object Gamma:
  inline def apply[F[_], A](shape: A, rate: A)(using g: Gamma[F, A]): F[A] =
    g.gamma(shape, rate)

  trait Default[F[_], A](using A: MultiplicativeMonoid[A])
      extends Gamma[F, A],
        Exponential.Default[F, A]:
    def exponential(rate: A) = gamma(A.one, rate)
