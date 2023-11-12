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

package schrodinger

import schrodinger.unsafe.Rng

/** Provides the capability to "dispatch" an independent rng that can be used in unsafe lands.
  */
trait RngDispatcher[F[_]] {
  type S
  def rng: Rng[S]
  def dispatch: F[S]
}

object RngDispatcher {
  type Aux[F[_], S0] = RngDispatcher[F] { type S = S0 }

  inline def apply[F[_]](using d: RngDispatcher[F]): d.type = d
}
