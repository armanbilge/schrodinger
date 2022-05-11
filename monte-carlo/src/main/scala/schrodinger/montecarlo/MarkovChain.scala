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

trait MarkovChain[F[_], G[_]]:
  def markovChain[A](initial: A)(transition: A => G[A]): F[A]

object MarkovChain:
  inline def apply[F[_], G[_], A](initial: A)(transition: A => G[A])(
      using mc: MarkovChain[F, G]
  ): F[A] = mc.markovChain(initial)(transition)

  given [F[_], A]: MarkovChain[Stream[F, _], F] with
    def markovChain[A](initial: A)(transition: A => F[A]) =
      Stream.iterateEval(initial)(transition)
