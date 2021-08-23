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

type Uniform[S, A] = [F[_]] =>> Distribution[F, Uniform.Params[S], A]

object Uniform:
  final case class Params[S](support: S)

  inline def apply[F[_], S, A](support: S)(using u: Uniform[S, A][F]): F[A] = u(Params(support))
  inline def standard[F[_], A](using u: Uniform[Unit, A][F]): F[A] = u(Params(()))

  given schrodingerKernelStandardUniformForInt[F[_]: Random]: Uniform[Unit, Int][F] with
    override def apply(support: Params[Unit]): F[Int] = Random[F].int

  given schrodingerKernelStandardUniformForLong[F[_]: Random]: Uniform[Unit, Long][F] with
    override def apply(support: Params[Unit]): F[Long] = Random[F].long
