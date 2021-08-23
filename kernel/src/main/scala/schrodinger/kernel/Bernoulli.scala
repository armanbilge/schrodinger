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

type Bernoulli[R, B] = [F[_]] =>> Distribution[F, Bernoulli.Params[R], B]

object Bernoulli:
  final case class Params[R](successProbability: R)

  inline def fair[F[_], B](using b: Bernoulli[0.5, B][F]): F[B] = b(Params(0.5))
  inline def apply[F[_], R, B](successProbability: R)(using b: Bernoulli[R, B][F]): F[B] =
    b(Params(successProbability))
