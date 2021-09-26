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

type Multinomial[S, N, X] = [F[_]] =>> Distribution[F, Multinomial.Params[S, N], X]

object Multinomial:
  type Aux[F[_], S, N, X] = Distribution[F, Multinomial.Params[S, N], X]

  final case class Params[+S, +N](support: S, trials: N)

  inline def apply[F[_], S, N, X](support: S, trials: N)(using m: Aux[F, S, N, X]): F[X] =
    m(Params(support, trials))
