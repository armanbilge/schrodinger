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

package schrodinger.montecarlo

import cats.Eq
import litter.{CommutativeZeroMonoid, ZeroMonoid}
import org.scalacheck.{Arbitrary, Cogen}
import schrodinger.montecarlo.Weighted.{Heavy, Weightless}

trait WeightedTestInstances {

  implicit object intZeroMonoid extends CommutativeZeroMonoid[Int] {
    override def empty: Int = 1
    override def absorbing: Int = 0
    override def combine(x: Int, y: Int): Int = x * y
  }

  implicit def schrodingerTestKitArbitraryForWeighted[
      W: Arbitrary: Eq: ZeroMonoid,
      A: Arbitrary]: Arbitrary[Weighted[W, A]] =
    Arbitrary(
      for {
        w <- Arbitrary.arbitrary[W]
        a <- Arbitrary.arbitrary[A]
      } yield Weighted(w, a)
    )

  implicit def schrodingerTestKitCogenForWeighted[A](
      implicit ev: Cogen[Option[A]]): Cogen[Weighted[Int, A]] =
    Cogen[Option[A]].contramap {
      case Heavy(_, a) => Some(a)
      case Weightless(_) => None
    }

  implicit def schrodingerTestKitArbitraryForWeightedT[F[_], W, A](
      implicit ev: Arbitrary[F[Weighted[W, A]]]): Arbitrary[WeightedT[F, W, A]] =
    Arbitrary(ev.arbitrary.map(WeightedT(_)))

  implicit def schrodingerTestKitCogenForWeightedT[F[_], A](
      implicit ev: Cogen[F[Weighted[Int, A]]]): Cogen[WeightedT[F, Int, A]] =
    ev.contramap(_.value)

}
