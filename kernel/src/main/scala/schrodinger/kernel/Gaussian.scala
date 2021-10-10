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

type GenGaussian[M, S, X] = [F[_]] =>> Distribution[F, Gaussian.Params[M, S], X]
type Gaussian[R] = [F[_]] =>> GenGaussian[R, R, R][F]

object Gaussian:
  final case class Params[+M, +S](mean: M, standardDeviation: S)

  inline def standard[F[_], X](using g: GenGaussian[0, 1, X][F]): F[X] =
    g(Params(0, 1))

  inline def apply[F[_], R](mean: R, standardDeviation: R)(using g: Gaussian[R][F]): F[R] =
    g(Params(mean, standardDeviation))
