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

package schrodinger.distributions

import cats.Functor
import schrodinger.RandomT

final class Exponential[F[_], S, A] private[distributions] (impl: ExponentialImpl[F, S, A])
    extends Serializable {
  def apply: RandomT[F, S, A] = impl.apply
}

object Exponential {
  def standard[F[_], S](implicit E: Exponential[F, S, Double]): RandomT[F, S, Double] =
    E.apply

  def apply[F[_]: Functor, S](rate: Double)(
      implicit E: Exponential[F, S, Double]): RandomT[F, S, Double] =
    standard.map(_ / rate)

  implicit def schrodingerDistributionsExponential[F[_], S, A](
      implicit E: PrioritizeGenerator[ExponentialImpl[F, S, A]]): Exponential[F, S, A] =
    new Exponential(E.join)
}

trait ExponentialImpl[F[_], S, A] extends Serializable {
  def apply: RandomT[F, S, A]
}

object ExponentialImpl {
  implicit def schrodingerDistributionsExponentialForDouble[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Double]): ExponentialImpl[F, S, Double] =
    new ExponentialImpl[F, S, Double] {
      override def apply: RandomT[F, S, Double] =
        U(()).map(U => -math.log(1 - U))
    }
}
