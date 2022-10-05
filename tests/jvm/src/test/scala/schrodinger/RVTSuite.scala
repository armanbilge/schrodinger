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

import cats.Alternative
import cats.CommutativeMonad
import cats.Eval
import cats.FunctorFilter
import cats.Order
import cats.data.OptionT
import cats.effect.Async
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Sync
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.CommutativeMonadTests
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.FunctorFilterTests
import cats.laws.discipline.SerializableTests
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Prop
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.testkit.Confidence
import schrodinger.testkit.RVTestkit
import schrodinger.unsafe.rng.SplitMix

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

class RVTSuite extends DisciplineSuite, RVTestkit:

  override protected def scalaCheckInitialSeed: String =
    "Q1J0q5oq1vByvYnjzXvwOZDzPP3aEJPeh_Dz1wXDDOJ="

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
