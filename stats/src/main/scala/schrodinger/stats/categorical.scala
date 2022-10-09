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
package stats

import algebra.ring.Rig
import cats.Applicative
import cats.Reducible
import cats.syntax.all.*
import schrodinger.kernel.Categorical

object categorical:
  given [F[_]: Applicative, G[_]: Reducible, A](using
      A: Rig[A],
  ): Categorical[Density[F, A, _], G[A], Long] with
    def categorical(support: G[A]) =
      Density(support.get(_).getOrElse(A.zero).pure)
