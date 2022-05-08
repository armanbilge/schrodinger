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

import cats.Functor
import cats.Invariant
import cats.Reducible
import cats.kernel.Hash
import cats.syntax.all.*

trait DiscreteUniform[F[_]] extends FairBernoulli[F]:
  def discreteUniform(n: Long): F[Long]

object DiscreteUniform:
  inline def apply[F[_]](n: Long)(using u: DiscreteUniform[F]): F[Long] =
    u.discreteUniform(n)

  def apply[F[_]: DiscreteUniform, G[_]: Reducible, A: Hash](support: G[A])(
      using F: Invariant[F]): F[A] =
    F match
      case given Functor[f] =>
        apply(support.size).map(support.get(_).get)
      case _ =>
        val inv = support.toIterable.view.zipWithIndex.toMap
        apply(support.size).imap(support.get(_).get)(inv.getOrElse(_, -1))

  trait Default[F[_]: Invariant] extends DiscreteUniform[F]:
    def fairBernoulli = discreteUniform(2).imap {
      case 0 => false
      case 1 => true
    } { if _ then 1 else 0 }
