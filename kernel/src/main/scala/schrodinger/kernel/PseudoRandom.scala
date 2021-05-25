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

/**
 * A pseudo-random `F` is a random that can be transformed deterministically to a `G` via a seed `S`.
 */
trait PseudoRandom[F[_]] extends Random[F] {
  type G[_]
  type S
  def simulate[A](fa: F[A])(seed: S): G[A]
}

object PseudoRandom {
  type Aux[F[_], G0[_], S0] = PseudoRandom[F] {
    type G[A] = G0[A]
    type S = S0
  }

  def apply[F[_]](implicit F: PseudoRandom[F]): F.type = F
}