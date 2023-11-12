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

package schrodinger.stats

import algebra.ring.Semifield
import cats.Applicative
import cats.syntax.all.*
import schrodinger.kernel.DiscreteUniform
import schrodinger.math.syntax.*

object discreteUniform {
  given [F[_]: Applicative, A](using A: Semifield[A]): DiscreteUniform[Density[F, A, _], Long]
  with {
    def discreteUniform(n: Long) = {
      val zero = A.zero.pure
      val p = A.fromBigInt(n).reciprocal.pure
      Density(i => if 0 <= i & i < n then p else zero)
    }
  }
}
