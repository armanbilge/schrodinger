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

import algebra.ring.Semifield
import cats.Reducible
import cats.data.NonEmptyList
import cats.syntax.all.*
import schrodinger.math.syntax.*

trait Categorical[F[_], P] extends DiscreteUniform[F], Bernoulli[F, P]:
  def categorical[G[_]: Reducible, A](support: G[(A, P)]): F[A]

object Categorical:
  inline def apply[F[_], G[_]: Reducible, A, P](support: G[(A, P)])(
      using c: Categorical[F, P]
  ): F[A] = c.categorical(support)

  trait Default[F[_], P](using P: Semifield[P])
      extends Categorical[F, P],
        DiscreteUniform.Default[F],
        Bernoulli.Default[F, P]:

    override def fairBernoulli = discreteUniform(NonEmptyList.of(false, true))

    def discreteUniform[G[_]: Reducible, A](support: G[A]) =
      val p = P.fromBigInt(support.size).reciprocal
      categorical(support.reduceMapK(a => NonEmptyList.one(a -> p)))
