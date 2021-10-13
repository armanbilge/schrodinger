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

package schrodinger
package testkit

import cats.effect.SyncIO
import cats.laws.discipline.ExhaustiveCheck
import cats.{Applicative, Eq, Eval}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.kernel.Bernoulli
import schrodinger.random.all.given
import schrodinger.unsafe.rng.SplitMix
import RVTEqSpec.DifferentRandoms

import scala.concurrent.duration.{DurationLong, FiniteDuration}

object RVTEqSpec:
  final case class DifferentRandoms[S](
      dist1: RVT[SyncIO, S, Boolean],
      dist2: RVT[SyncIO, S, Boolean])

class RVTEqSpec extends Specification with ScalaCheck with RVTestInstances:

  given [A: Eq]: Eq[SyncIO[A]] = Eq.by(_.unsafeRunSync())

  given (SyncIO[Boolean] => Option[Boolean]) = _.attempt.unsafeRunSync().toOption

  given Arbitrary[DifferentRandoms[SplitMix]] = Arbitrary(
    for p <- Gen.choose[Double](0, 0.25)
    yield DifferentRandoms(
      Bernoulli[RVT[SyncIO, SplitMix, _], Double, Boolean](p),
      Bernoulli[RVT[SyncIO, SplitMix, _], Double, Boolean](1 - p))
  )

  given ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix.fromTime[SyncIO].unsafeRunSync()))

  given Confidence = Confidence(1000, 0.95)

  "RVTEq" should {
    "recognize equivalent distributions" in {
      prop { (dist: RVT[SyncIO, SplitMix, Boolean]) =>
        Eq[RVT[SyncIO, SplitMix, Boolean]].eqv(dist, dist)
      }
    }

    "recognize non-equivalent distributions" in {
      prop { (different: DifferentRandoms[SplitMix]) =>
        import different.*
        Eq[RVT[SyncIO, SplitMix, Boolean]].neqv(dist1, dist2)
      }
    }
  }
