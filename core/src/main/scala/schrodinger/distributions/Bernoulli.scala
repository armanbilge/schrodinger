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

final class Bernoulli[F[_], S, A] private[distributions] (impl: BernoulliImpl[F, S, A])
    extends Serializable {
  def apply(a: A): RandomT[F, S, Boolean] = impl(a)
}

object Bernoulli {

  def fair[F[_], S](implicit B: Bernoulli[F, S, Unit]): RandomT[F, S, Boolean] =
    B(())

  /**
   * @param p the probability of `true`
   */
  def apply[F[_], S](p: Double)(implicit B: Bernoulli[F, S, Double]): RandomT[F, S, Boolean] =
    B(p)

  implicit def schrodingerDistributionsBernoulli[F[_], S, A](
      implicit impl: PrioritizeGenerator[BernoulliImpl[F, S, A]]): Bernoulli[F, S, A] =
    new Bernoulli(impl.join)
}

trait BernoulliImpl[F[_], S, A] extends Serializable {
  def apply(args: A): RandomT[F, S, Boolean]
}

object BernoulliImpl {
  implicit def schrodingerDistributionsFairBernoulli[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Int]): BernoulliImpl[F, S, Unit] =
    new BernoulliImpl[F, S, Unit] {
      override def apply(args: Unit): RandomT[F, S, Boolean] =
        U(()).map(_ >= 0)
    }

  implicit def schrodingerDistributionsBernoulliForDouble[F[_]: Functor, S](
      implicit U: Uniform[F, S, Unit, Double]): BernoulliImpl[F, S, Double] =
    new BernoulliImpl[F, S, Double] {
      override def apply(p: Double): RandomT[F, S, Boolean] =
        U(()).map(_ < p)
    }
}
