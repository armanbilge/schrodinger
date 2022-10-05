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

import cats.ApplicativeError
import cats.Monad
import cats.effect.kernel.Async
import cats.effect.kernel.testkit.AsyncGenerators
import cats.effect.kernel.testkit.GenK
import cats.effect.kernel.testkit.MonadGenerators
import cats.kernel.Eq
import cats.kernel.Order
import cats.laws.discipline.ExhaustiveCheck
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.testkit.Confidence
import schrodinger.kernel.testkit.PseudoRandomEq
import schrodinger.kernel.testkit.RandomGenerators
import schrodinger.kernel.testkit.SimulationResult
import schrodinger.unsafe.rng.Rng
import schrodinger.unsafe.rng.SplittableRng

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

trait RVTestkit extends RVTestkitLowPriority, cats.effect.testkit.TestInstances:

  export schrodinger.kernel.testkit.random

  given [F[_]: Monad, S, A: Eq](using
      PseudoRandom.Aux[RVT[F, S, _], F, S],
      ExhaustiveCheck[S],
      Confidence,
      Eq[F[SimulationResult[A]]],
  ): Eq[RVT[F, S, A]] =
    PseudoRandomEq[RVT[F, S, _], F, S, A]

  given [F[_]: Async, S: SplittableRng: ExhaustiveCheck, A: Arbitrary: Cogen](using
      Cogen[F[Unit]],
      Ticker,
  ): Arbitrary[RVT[F, S, A]] =
    val generators = new RandomGenerators[RVT[F, S, _]] with AsyncGenerators[RVT[F, S, _]]:
      val F: Async[RVT[F, S, _]] = Async[RVT[F, S, _]]
      val arbitraryE: Arbitrary[Throwable] = arbitraryThrowable
      val cogenE: Cogen[Throwable] = Cogen[Throwable]
      val arbitraryEC: Arbitrary[ExecutionContext] = arbitraryExecutionContext
      val arbitraryFD: Arbitrary[FiniteDuration] = arbitraryFiniteDuration
      val cogenFU: Cogen[RVT[F, S, Unit]] = Cogen[RVT[F, S, Unit]]

      override def recursiveGen[B: Arbitrary: Cogen](deeper: GenK[RVT[F, S, _]]) =
        super
          .recursiveGen[B](deeper)
          .filterNot(
            _._1 == "racePair",
          )

    Arbitrary(generators.generators)

  given [F[_]: Monad, S, A](using
      pseudo: PseudoRandom.Aux[RVT[F, S, _], F, S],
      seeds: ExhaustiveCheck[S],
      cogen: Cogen[F[A]],
  ): Cogen[RVT[F, S, A]] =
    Cogen[List[F[A]]].contramap(rv => seeds.allValues.map(pseudo.simulate(rv)))

sealed private[testkit] trait RVTestkitLowPriority:
  given [F[_]: Monad, S, A: Arbitrary: Cogen](using
      PseudoRandom.Aux[RVT[F, S, _], F, S],
  ): Arbitrary[RVT[F, S, A]] =
    val generators = new RandomGenerators[RVT[F, S, _]] with MonadGenerators[RVT[F, S, _]]:
      override val maxDepth = 3
      val F = Monad[RVT[F, S, _]]

    Arbitrary(generators.generators)

  given [F[_]: Monad, S, A](using
      pseudo: PseudoRandom.Aux[RVT[F, S, _], F, S],
      seeds: ExhaustiveCheck[S],
      orderF: Order[F[A]],
  ): Order[RVT[F, S, A]] =
    Order.by[RVT[F, S, A], List[F[A]]](rv => seeds.allValues.map(pseudo.simulate(rv)))
