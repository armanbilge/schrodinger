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
import cats.Eq
import cats.Eval
import cats.FunctorFilter
import cats.Monad
import cats.Order
import cats.data.OptionT
import cats.effect.Async
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.kernel.Sync
import cats.effect.kernel.testkit.AsyncGenerators
import cats.effect.kernel.testkit.GenK
import cats.effect.kernel.testkit.MonadGenerators
import cats.effect.kernel.testkit.SyncGenerators
import cats.effect.laws.AsyncTests
import cats.effect.laws.SyncTests
import cats.effect.testkit.TestInstances
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.AlignTests
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.CommutativeMonadTests
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.FunctorFilterTests
import cats.laws.discipline.SemigroupKTests
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Prop
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.testkit.Confidence
import schrodinger.testkit.RVTestkit
import schrodinger.unsafe.rng.Rng
import schrodinger.unsafe.rng.SplitMix
import schrodinger.unsafe.rng.SplittableRng

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

// Testing for equivalence of distributions is just too ambitious
class RVIODeterministicSuite extends DisciplineSuite, cats.effect.testkit.TestInstances:

  given Ticker = Ticker()
  given seeds: ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix(1234567890L, SplitMix.GoldenGamma)))

  given Order[RVIO[SplitMix, FiniteDuration]] =
    Order.by[RVIO[SplitMix, FiniteDuration], List[IO[FiniteDuration]]](rv =>
      seeds.allValues.map(rv.simulate))

  given [A](using seeds: ExhaustiveCheck[SplitMix], orderF: Eq[IO[A]]): Eq[RVIO[SplitMix, A]] =
    Eq.by[RVIO[SplitMix, A], List[IO[A]]](rv =>
      seeds
        .allValues
        .map(
          rv.simulate(_)
            .attempt
            .flatTap(x => x.left.toOption.fold(IO.unit)(e => IO(() => e.printStackTrace())))
            .flatMap(IO.fromEither)))

  given [A: Arbitrary: Cogen]: Arbitrary[RVIO[SplitMix, A]] =
    val generators =
      new AsyncGenerators[RVIO[SplitMix, _]]:
        val F: Async[RVIO[SplitMix, _]] = Async[RVIO[SplitMix, _]]
        val arbitraryE: Arbitrary[Throwable] = arbitraryThrowable
        val cogenE: Cogen[Throwable] = Cogen[Throwable]
        val arbitraryEC: Arbitrary[ExecutionContext] = arbitraryExecutionContext
        val arbitraryFD: Arbitrary[FiniteDuration] = arbitraryFiniteDuration
        val cogenFU: Cogen[RVIO[SplitMix, Unit]] = Cogen[RVIO[SplitMix, Unit]]
        override def recursiveGen[B: Arbitrary: Cogen](deeper: GenK[RVIO[SplitMix, _]]) =
          super.recursiveGen[B](deeper).filterNot(_._1 == "racePair")

    Arbitrary(generators.generators)

  given [A](using cogen: Cogen[IO[A]]): Cogen[RVIO[SplitMix, A]] =
    Cogen[List[IO[A]]].contramap(rv => seeds.allValues.map(rv.simulate))

  given Conversion[RVIO[SplitMix, Boolean], Prop] =
    rv => ioBooleanToProp(seeds.allValues.forallM(rv.simulate(_)))

  given RVIO.Algebra[SplitMix] = RVIO.algebra[SplitMix].syncStep.unsafeRunSync().toOption.get

  checkAll("RVIO", AsyncTests[RVIO[SplitMix, _]].applicative[Int, Int, Int])
  checkAll("RVIO", AsyncTests[RVIO[SplitMix, _]].async[Int, Int, Int](100.millis))
  checkAll("RVIO", MonoidTests[RVIO[SplitMix, Int]].monoid)
  checkAll("RVIO", SemigroupKTests[RVIO[SplitMix, _]].semigroupK[Int])
  checkAll("RVIO", AlignTests[RVIO[SplitMix, _]].align[Int, Int, Int, Int])
