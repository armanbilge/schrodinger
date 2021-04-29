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

package schrodinger.testkit

import cats.Eq
import org.scalacheck.{Arbitrary, Cogen}
import schrodinger.Binoid
import schrodinger.data.Weighted.{Heavy, Weightless}
import schrodinger.data.{Weighted, WeightedT}

object weighted {

  implicit def schrodingerTestKitArbitraryForWeighted[W: Arbitrary: Eq: Binoid, A: Arbitrary]
      : Arbitrary[Weighted[W, A]] =
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
