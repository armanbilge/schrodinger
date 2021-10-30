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

type GenBeta[A, B, X] = [F[_]] =>> Distribution[F, Beta.Params[A, B], X]
type Beta[R] = [F[_]] =>> GenBeta[R, R, R][F]

object Beta:
  final case class Params[+A, +B](alpha: A, beta: B)

  inline def apply[F[_], A, B, X](alpha: A, beta: B)(using b: GenBeta[A, B, X][F]): F[X] =
    b(Params(alpha, beta))
