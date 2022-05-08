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

trait Multinomial[F[_], V, P] extends Categorical[F[_], V, P]:
  def multinomial(probabilites: V, trials: Long): F[Vector[Long]]

object Multinomial:
  inline def apply[F[_], V, P](probabilites: V, trials: Long)(
      using m: Multinomial[F, V, P]
  ): F[Vector[Long]] = m.multinomial(probabilites, trials)
