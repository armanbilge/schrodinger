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

type Uniform[S, X] = [F[_]] =>> Distribution[F, Uniform.Params[S], X]
type UniformRange[F[_]] = Uniform[Range, Int][F]

object Uniform:
  final case class Params[+S](support: S)

  inline def apply[F[_], S, X](support: S)(using u: Uniform[S, X][F]): F[X] = u(Params(support))
