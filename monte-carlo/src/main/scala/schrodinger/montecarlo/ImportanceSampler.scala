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

package schrodinger.montecarlo

import algebra.ring.Semifield
import cats.Monad
import cats.kernel.Eq
import cats.syntax.all.*
import schrodinger.kernel.Categorical
import schrodinger.kernel.Distribution
import schrodinger.kernel.UnnormalizedDensity
import schrodinger.math.syntax.*

type ImportanceSampler[P, Q, X] = [F[_]] =>> Distribution[F, ImportanceSampler.Params[P, Q], X]

object ImportanceSampler:
  final case class Params[P, Q](target: P, proposal: Q, samples: Int)

  given [F[_]: Monad: Categorical[Seq[(Weighted[R, X], R)], Weighted[R, X]], P, Q, X, R: Eq](
      using Q: Distribution[WeightedT[F, R, _], Q, X],
      P: Distribution[UnnormalizedDensity[F, R], P, X])(
      using R: Semifield[R]): ImportanceSampler[P, Q, X][[A] =>> F[WeightedT[F, R, A]]] =
    case Params(target, proposal, n) =>
      val q = Q(proposal)
      val p = P(target)

      val qs: Seq[WeightedT[F, R, X]] = new IndexedSeq[WeightedT[F, R, X]]:
        def length = n
        def apply(i: Int) = q

      qs.traverse(_.importanceF(p).value).map { samples =>
        import Weighted.*
        val marginal = R.sum(samples.view.map(_.weight)) / R.sumN(R.one, n)
        WeightedT(
          Categorical(samples.fproduct(_.weight)).map {
            case Heavy(_, d, x) => Weighted(d / marginal, x)
            case weightless @ Weightless(_) => weightless
          }
        )
      }
