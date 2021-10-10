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

type Bernoulli[P, X] = [F[_]] =>> Distribution[F, Bernoulli.Params[P], X]

object Bernoulli:
  final case class Params[+P](successProbability: P)

  inline def fair[F[_], X](using b: Bernoulli[0.5, X][F]): F[X] = b(Params(0.5))
  inline def apply[F[_], P, X](successProbability: P)(using b: Bernoulli[P, X][F]): F[X] =
    b(Params(successProbability))
