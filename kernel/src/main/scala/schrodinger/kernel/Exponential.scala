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

type GenExponential[R, X] = [F[_]] =>> Distribution[F, Exponential.Params[R], X]
type Exponential[R] = [F[_]] =>> GenExponential[R, R][F]

object Exponential:
  type GenAux[F[_], R, X] = Distribution[F, Exponential.Params[R], X]
  type Aux[F[_], R] = Distribution[F, Exponential.Params[R], R]

  final case class Params[+R](rate: R)

  inline def standard[F[_], X](using e: GenAux[F, 1, X]): F[X] = e(Params(1))

  inline def apply[F[_], R](rate: R)(using e: Aux[F, R]): F[R] =
    e(Params(rate))
