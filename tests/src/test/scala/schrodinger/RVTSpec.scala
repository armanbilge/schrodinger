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
import cats.effect.Async
import cats.effect.IO
import cats.effect.SyncIO
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.instances.option.given
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.CommutativeMonadTests
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.FunctorFilterTests
import cats.laws.discipline.SerializableTests
import cats.syntax.foldable.given
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.laws.PseudoRandomTests
import schrodinger.rng.SplitMix
import schrodinger.testkit.Confidence
import schrodinger.testkit.Discrete
import schrodinger.testkit.RVTestInstances

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

class RVTSpec extends Specification, Discipline, ScalaCheck, TestInstances, RVTestInstances:

  sequential

  given Ticker = Ticker()
  given Confidence = Confidence(1000, 0.99)

  given seeds: ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix.fromTime[SyncIO].unsafeRunSync()))

  given discreteEitherUnitBoolean: Discrete[Either[Unit, Boolean]] =
    Discrete.instance(List(Left(()), Right(false), Right(true)))
  given discreteEitherBooleanUnit: Discrete[Either[Boolean, Unit]] =
    Discrete.instance(List(Right(()), Left(false), Left(true)))

  given (using Ticker): Conversion[RVT[IO, SplitMix, Boolean], Prop] =
    sbool => ioBooleanToProp(seeds.allValues.forallM(sbool.simulate(_)))

  given (using Ticker): Conversion[IO[Boolean], Option[Boolean]] = ioa =>
    unsafeRun(ioa).fold(None, _ => None, identity)

  given (using Ticker): Order[RVT[IO, SplitMix, FiniteDuration]] =
    Order.by(_.simulate(seeds.allValues.head))

  checkAll(
    "PseudoRandom[RVT]",
    PseudoRandomTests[RV[SplitMix, _], Eval, SplitMix].pseudoRandom[Boolean])
  checkAll(
    "PseudoRandom[RVT]",
    SerializableTests.serializable(PseudoRandom[RVT[IO, SplitMix, _]]))

  checkAll(
    "Async[RVT]",
    AsyncTests[RVT[IO, SplitMix, _]].async[Boolean, Boolean, Boolean](10.millis))
  checkAll("Async[RVT]", SerializableTests.serializable(Async[RVT[IO, SplitMix, _]]))

  checkAll(
    "CommutativeMonad[RVT]",
    CommutativeMonadTests[RVT[Option, SplitMix, _]].commutativeMonad[Boolean, Boolean, Boolean])
  checkAll(
    "CommutativeMonad[RVT]",
    SerializableTests.serializable(CommutativeMonad[RVT[Option, SplitMix, _]]))

  checkAll(
    "FunctorFilter[RVT]",
    FunctorFilterTests[RVT[Option, SplitMix, _]].functorFilter[Boolean, Boolean, Boolean])
  checkAll(
    "FunctorFilter[RVT]",
    SerializableTests.serializable(FunctorFilter[RVT[Option, SplitMix, _]]))

  checkAll(
    "Alternative[RVT]",
    AlternativeTests[RVT[Option, SplitMix, _]].alternative[Boolean, Boolean, Boolean])
  checkAll(
    "Alternative[RVT]",
    SerializableTests.serializable(Alternative[RVT[Option, SplitMix, _]]))
