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

trait Uniform[F[_], S, A] {
  def apply(support: S): F[A]
}

object Uniform {
  def apply[F[_], S, A](support: S)(implicit u: Uniform[F, S, A]): F[A] = u(support)
  def standard[F[_], A](implicit u: Uniform[F, Unit, A]): F[A] = u(())

  implicit def schrodingerKernelStandardUniformForInt[F[_]: Random]: Uniform[F, Unit, Int] =
    new Uniform[F, Unit, Int] {
      override def apply(support: Unit): F[Int] = Random[F].int
    }

  implicit def schrodingerKernelStandardUniformForLong[F[_]: Random]: Uniform[F, Unit, Long] =
    new Uniform[F, Unit, Long] {
      override def apply(support: Unit): F[Long] = Random[F].long
    }
}
