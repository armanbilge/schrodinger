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

import algebra.ring.Semiring
import cats.Functor
import cats.Invariant
import cats.Reducible
import cats.kernel.Hash
import cats.syntax.all.*

trait Multinomial[F[_], P] extends Categorical[F[_], P]:
  def multinomial[G[_]: Reducible](probabilites: G[P], trials: Long): F[Vector[Long]]

object Multinomial:
  inline def apply[F[_], P, G[_]: Reducible](probabilites: G[P], trials: Long)(
      using m: Multinomial[F, P]): F[Vector[Long]] =
    m.multinomial(probabilites, trials)

  trait Default[F[_], P] extends Multinomial[F, P], Categorical.Default[F, P]:
    def categorical[G[_]: Reducible](probabilites: G[P], trials: Long): F[Vector[Long]]
