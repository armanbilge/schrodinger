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

import cats.FunctorFilter
import cats.data.OptionT
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Sync
import cats.effect.syntax.all.*
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.CommutativeMonadTests
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.FunctorFilterTests
import cats.syntax.all.*
import munit.CatsEffectSuite
import munit.DisciplineSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF.*
import schrodinger.kernel.Gaussian
import schrodinger.kernel.testkit.Confidence
import schrodinger.testkit.RVTestkit
import schrodinger.unsafe.SplitMix

class RVTSuite extends CatsEffectSuite, DisciplineSuite, ScalaCheckEffectSuite, RVTestkit:

  override protected def scalaCheckInitialSeed: String =
    "Q1J0q5oq1vByvYnjzXvwOZDzPP3aEJPeh_Dz1wXDDOJ="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if System.getProperty("java.vm.name") == "Scala Native" then 10 else 100,
    )

  given Confidence = Confidence(1000, 0.9, 0.9)

  given seeds: ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix(1234567890L, SplitMix.GoldenGamma)))

  given Simulator[Option] with
    type G[A] = OptionT[SyncIO, A]
    given runtime: Sync[G] = Sync.syncForOptionT[SyncIO]
    def upgrade[A](fa: Option[A]): G[A] = OptionT.fromOption(fa)
    def downgrade[A](ga: G[A]): Option[A] = ga.value.unsafeRunSync()

  checkAll(
    "RVT",
    CommutativeMonadTests[RVT[Option, SplitMix, _]]
      .commutativeMonad[Boolean, Boolean, Boolean]
      .random,
  )

  checkAll(
    "RVT",
    FunctorFilterTests[RVT[Option, SplitMix, _]]
      .functorFilter[Boolean, Boolean, Boolean]
      .random,
  )

  checkAll(
    "RVT",
    AlternativeTests[RVT[Option, SplitMix, _]].alternative[Boolean, Boolean, Boolean].random,
  )

  test("streams are not identical") {
    forAllF { (seed: SplitMix) =>
      val nextLong = RVT.long[IO, SplitMix]
      val prog = nextLong *> nextLong.both(nextLong).evalMap { (left, right) =>
        IO(assert(clue(left) != clue(right)))
      }
      prog.simulate(seed)
    }
  }

  test("streams of gaussians are not identical") {
    forAllF { (seed: SplitMix) =>
      val nextGaussian = Gaussian.standard[RVT[IO, SplitMix, *], Double]
      val prog = nextGaussian *> nextGaussian.both(nextGaussian).evalMap { (left, right) =>
        IO(assert(clue(left) != clue(right)))
      }
      prog.simulate(seed)
    }
  }
