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

import fs2.Stream
import schrodinger.kernel.Distribution

type MarkovChain[P, A] = [F[_]] =>> Distribution[F, MarkovChain.Params[P, A], A]

object MarkovChain:
  final case class Params[P, A](initial: A, transition: A => P)

  inline def apply[F[_], P, A](initial: A)(transition: A => P)(
      using mc: MarkovChain[P, A][F]): F[A] =
    mc(Params(initial, transition))

  given [F[_], P, A](using P: Distribution[F, P, A]): MarkovChain[P, A][Stream[F, _]] =
    case Params(initial, transition) => Stream.iterateEval(initial)(a => P(transition(a)))
