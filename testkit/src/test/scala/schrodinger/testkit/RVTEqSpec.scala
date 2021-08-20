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

import cats.effect.kernel.Clock
import cats.laws.discipline.ExhaustiveCheck
import cats.{Applicative, Eq, Eval}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import schrodinger.RV
import schrodinger.kernel.Bernoulli
import schrodinger.random.all.given
import schrodinger.rng.SplitMix
import schrodinger.rng.SplitMix.given
import schrodinger.testkit.RVTEqSpec.DifferentRandoms

import scala.concurrent.duration.{DurationLong, FiniteDuration}

object RVTEqSpec:
  final case class DifferentRandoms[S](dist1: RV[S, Boolean], dist2: RV[S, Boolean])

class RVTEqSpec extends Specification with ScalaCheck with RVTestInstances:

  given (Eval[Boolean] => Some[Boolean]) = eval => Some(eval.value)

  given Clock[Eval] with
    override def applicative: Applicative[Eval] = Eval.catsBimonadForEval
    override def monotonic: Eval[FiniteDuration] = Eval.later(System.nanoTime().nanoseconds)
    override def realTime: Eval[FiniteDuration] = Eval.later(System.currentTimeMillis().millis)

  given Arbitrary[DifferentRandoms[SplitMix]] = Arbitrary(
    for p <- Gen.choose[Double](0, 0.25)
    yield DifferentRandoms(
      Bernoulli[RV[SplitMix, _], Double, Boolean](p),
      Bernoulli[RV[SplitMix, _], Double, Boolean](1 - p))
  )

  given ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix.fromTime[Eval].value))

  given Confidence = Confidence(1000, 0.95)

  "RVTEq" should {
    "recognize equivalent distributions" in {
      prop { (dist: RV[SplitMix, Boolean]) => Eq[RV[SplitMix, Boolean]].eqv(dist, dist) }
    }

    "recognize non-equivalent distributions" in {
      prop { (different: DifferentRandoms[SplitMix]) =>
        import different.*
        Eq[RV[SplitMix, Boolean]].neqv(dist1, dist2)
      }
    }
  }
