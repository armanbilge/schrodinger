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

type Gamma[R] = [F[_]] =>> Distribution[F, Gamma.Params[R], R]

object Gamma:
  final case class Params[R](shape: R, rate: R)

  inline def apply[F[_], R](shape: R, rate: R)(using g: Gamma[R][F]): F[R] =
    g(Params(shape, rate))
