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

import cats.{Alternative, CommutativeMonad, Eval, FunctorFilter, Order}
import cats.effect.{Async, IO, SyncIO}
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.laws.discipline.{
  AlternativeTests,
  CommutativeMonadTests,
  ExhaustiveCheck,
  FunctorFilterTests,
  SerializableTests
}
import cats.instances.option._
import cats.syntax.foldable._
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.kernel.PseudoRandom
import schrodinger.kernel.laws.PseudoRandomTests
import schrodinger.rng.SplitMix
import schrodinger.testkit.{Confidence, Discrete, RVTestInstances}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class RVTSpec
    extends Specification
    with Discipline
    with ScalaCheck
    with TestInstances
    with RVTestInstances {

  sequential

  implicit val ticker: Ticker = Ticker()
  implicit val confidence: Confidence = Confidence(1000, 0.99)

  implicit val seeds: ExhaustiveCheck[SplitMix] =
    ExhaustiveCheck.instance(List(SplitMix.fromTime[SyncIO].unsafeRunSync()))

  implicit val discreteEitherUnitBoolean: Discrete[Either[Unit, Boolean]] =
    Discrete.instance(List(Left(()), Right(false), Right(true)))
  implicit val discreteEitherBooleanUnit: Discrete[Either[Boolean, Unit]] =
    Discrete.instance(List(Right(()), Left(false), Left(true)))

  implicit def execRVTIO(sbool: RVT[IO, SplitMix, Boolean])(implicit ticker: Ticker): Prop =
    ioBooleanToProp(seeds.allValues.forallM(sbool.simulate(_)))

  implicit def execIOBoolean(ioa: IO[Boolean])(implicit ticker: Ticker): Option[Boolean] =
    unsafeRun(ioa).fold(None, _ => None, identity)

  implicit def ordDTFD(implicit ticker: Ticker): Order[RVT[IO, SplitMix, FiniteDuration]] =
    Order.by(_.simulate(seeds.allValues.head))

  checkAll(
    "PseudoRandom[RVT]",
    PseudoRandomTests[RV[SplitMix, *], Eval, SplitMix].pseudoRandom[Boolean])
  checkAll(
    "PseudoRandom[RVT]",
    SerializableTests.serializable(PseudoRandom[RVT[IO, SplitMix, *]]))

  checkAll(
    "Async[RVT]",
    AsyncTests[RVT[IO, SplitMix, *]].async[Boolean, Boolean, Boolean](10.millis))
  checkAll("Async[RVT]", SerializableTests.serializable(Async[RVT[IO, SplitMix, *]]))

  checkAll(
    "CommutativeMonad[RVT]",
    CommutativeMonadTests[RVT[Option, SplitMix, *]].commutativeMonad[Boolean, Boolean, Boolean])
  checkAll(
    "CommutativeMonad[RVT]",
    SerializableTests.serializable(CommutativeMonad[RVT[Option, SplitMix, *]]))

  checkAll(
    "FunctorFilter[RVT]",
    FunctorFilterTests[RVT[Option, SplitMix, *]].functorFilter[Boolean, Boolean, Boolean])
  checkAll(
    "FunctorFilter[RVT]",
    SerializableTests.serializable(FunctorFilter[RVT[Option, SplitMix, *]]))

  checkAll(
    "Alternative[RVT]",
    AlternativeTests[RVT[Option, SplitMix, *]].alternative[Boolean, Boolean, Boolean])
  checkAll(
    "Alternative[RVT]",
    SerializableTests.serializable(Alternative[RVT[Option, SplitMix, *]]))

}
