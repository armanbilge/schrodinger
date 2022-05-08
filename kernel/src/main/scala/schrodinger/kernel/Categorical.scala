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
import algebra.ring.Semiring
import cats.Functor
import cats.Invariant
import cats.Reducible
import cats.data.NonEmptyVector
import cats.kernel.Hash
import cats.syntax.all.*
import schrodinger.math.syntax.*

trait Categorical[F[_], P] extends DiscreteUniform[F], Bernoulli[F, P]:
  def categorical[G[_]: Reducible](probabilites: G[P]): F[Long]

object Categorical:
  inline def apply[F[_], G[_]: Reducible, A, P](probabilites: G[P])(
      using c: Categorical[F, P]
  ): F[Long] = c.categorical(probabilites)

  def apply[F[_], P, G[_]: Functor: Reducible, A: Hash](
      support: G[(A, P)])(using F: Invariant[F], P: Semiring[P], c: Categorical[F, P]): F[A] =
    val probabilities = support.map(_._2)
    F match
      case given Functor[f] =>
        apply(probabilities).map(support.get(_).get._1)
      case _ =>
        val inv = support.toIterable.view.map(_._1).zipWithIndex.toMap
        apply(probabilities).imap(support.get(_).get._1)(inv.getOrElse(_, -1))

  trait Default[F[_], P](using P: Semifield[P])
      extends Categorical[F, P],
        DiscreteUniform.Default[F],
        Bernoulli.Default[F, P]:

    override def fairBernoulli = super.fairBernoulli

    def discreteUniform(n: Long) =
      val p = P.fromBigInt(n).reciprocal
      categorical(NonEmptyVector.fromVectorUnsafe(Vector.fill(n.toInt)(p)))
