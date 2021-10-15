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

import algebra.ring.Rig
import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

given [W: Arbitrary: Eq: Rig, A: Arbitrary]: Arbitrary[Weighted[W, A]] =
  Arbitrary(
    for {
      w <- Arbitrary.arbitrary[W]
      d <- Arbitrary.arbitrary[W]
      a <- Arbitrary.arbitrary[A]
    } yield Weighted(w, d, a)
  )

given [A](using cogen: Cogen[Option[A]]): Cogen[Weighted[Int, A]] =
  cogen.contramap {
    case Heavy(_, _, a) => Some(a)
    case Weightless(_) => None
  }

given [F[_], W, A](using arb: Arbitrary[F[Weighted[W, A]]]): Arbitrary[WeightedT[F, W, A]] =
  Arbitrary(arb.arbitrary.map(WeightedT(_)))

given [F[_], A](using cogen: Cogen[F[Weighted[Int, A]]]): Cogen[WeightedT[F, Int, A]] =
  cogen.contramap(_.value)
