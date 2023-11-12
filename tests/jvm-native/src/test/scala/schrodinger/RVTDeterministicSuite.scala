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

import cats.Eq
import cats.Order
import cats.effect.Async
import cats.effect.IO
import cats.effect.kernel.testkit.AsyncGenerators
import cats.effect.kernel.testkit.GenK
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Prop
import schrodinger.unsafe.SplitMix

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

// Testing for equivalence of distributions is just too ambitious
class RVTDeterministicSuite extends DisciplineSuite, cats.effect.testkit.TestInstances {

  given Ticker = Ticker()
  given seeds: ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix(1234567890L, SplitMix.GoldenGamma)))

  given Order[RVT[IO, SplitMix, FiniteDuration]] =
    Order.by[RVT[IO, SplitMix, FiniteDuration], List[IO[FiniteDuration]]](rv =>
      seeds.allValues.map(rv.simulate),
    )

  given [A](using
      seeds: ExhaustiveCheck[SplitMix],
      orderF: Eq[IO[A]],
  ): Eq[RVT[IO, SplitMix, A]] =
    Eq.by[RVT[IO, SplitMix, A], List[IO[A]]](rv => seeds.allValues.map(rv.simulate))

  given [A: Arbitrary: Cogen]: Arbitrary[RVT[IO, SplitMix, A]] = {
    val generators =
      new AsyncGenerators[RVT[IO, SplitMix, _]] {
        val F: Async[RVT[IO, SplitMix, _]] = Async[RVT[IO, SplitMix, _]]
        val arbitraryE: Arbitrary[Throwable] = arbitraryThrowable
        val cogenE: Cogen[Throwable] = Cogen[Throwable]
        val arbitraryEC: Arbitrary[ExecutionContext] = arbitraryExecutionContext
        val arbitraryFD: Arbitrary[FiniteDuration] = arbitraryFiniteDuration
        val cogenFU: Cogen[RVT[IO, SplitMix, Unit]] = Cogen[RVT[IO, SplitMix, Unit]]
        override def recursiveGen[B: Arbitrary: Cogen](deeper: GenK[RVT[IO, SplitMix, _]]) =
          super.recursiveGen[B](deeper).filterNot(_._1 == "racePair")
      }

    Arbitrary(generators.generators)
  }

  given [A](using cogen: Cogen[IO[A]]): Cogen[RVT[IO, SplitMix, A]] =
    Cogen[List[IO[A]]].contramap(rv => seeds.allValues.map(rv.simulate))

  given Conversion[RVT[IO, SplitMix, Boolean], Prop] =
    rv => ioBooleanToProp(seeds.allValues.forallM(rv.simulate(_)))

  checkAll("RVT", AsyncTests[RVT[IO, SplitMix, _]].async[Int, Int, Int](100.millis))
}
