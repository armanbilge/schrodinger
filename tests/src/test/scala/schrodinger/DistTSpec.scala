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

import cats.{Alternative, CommutativeMonad, FunctorFilter, Order}
import cats.effect.{Async, IO}
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
import cats.syntax.traverse._
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.generators.SplitMix
import schrodinger.generators.SplitMix.DefaultInstance
import schrodinger.testkit.{Confidence, Discrete}
import schrodinger.testkit.dist._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class DistTSpec extends Specification with Discipline with ScalaCheck with TestInstances {

  sequential

  implicit val confidence = Confidence(1000, 0.99)

  implicit val seeds = ExhaustiveCheck.instance(
    List
      .fill(1)(SplitMix.DefaultInstance.split)
      .sequence
      .runA(SplitMix.initialState(System.currentTimeMillis(), System.nanoTime()))
  )

  implicit val discreteEitherUnitBoolean: Discrete[Either[Unit, Boolean]] =
    Discrete.instance(List(Left(()), Right(false), Right(true)))
  implicit val discreteEitherBooleanUnit: Discrete[Either[Boolean, Unit]] =
    Discrete.instance(List(Right(()), Left(false), Left(true)))

  implicit def execDistTIO(sbool: DistT[IO, Boolean])(implicit ticker: Ticker): Prop =
    ioBooleanToProp(seeds.allValues.forallM(sbool.sample(_)))

  implicit def execIOBoolean(ioa: IO[Boolean])(implicit ticker: Ticker): Option[Boolean] =
    unsafeRun(ioa).fold(None, _ => None, identity)

  implicit def ordDTFD(implicit ticker: Ticker): Order[DistT[IO, FiniteDuration]] =
    Order.by(_.sample(seeds.allValues.head))

  {
    import schrodinger.effect.instances.dist._
    implicit val ticker = Ticker()
    checkAll(
      "Async[DistT]",
      AsyncTests[DistT[IO, *]].async[Boolean, Boolean, Boolean](10.millis))
    checkAll("Async[DistT]", SerializableTests.serializable(Async[DistT[IO, *]]))
  }

  checkAll(
    "CommutativeMonad[DistT]",
    CommutativeMonadTests[DistT[Option, *]].commutativeMonad[Boolean, Boolean, Boolean])
  checkAll(
    "CommutativeMonad[DistT]",
    SerializableTests.serializable(CommutativeMonad[DistT[Option, *]]))

  checkAll(
    "FunctorFilter[DistT]",
    FunctorFilterTests[DistT[Option, *]].functorFilter[Boolean, Boolean, Boolean])
  checkAll(
    "FunctorFilter[DistT]",
    SerializableTests.serializable(FunctorFilter[DistT[Option, *]]))

  // TODO This (correctly) hangs, can we run it with a timeout?
//  {
//    import schrodinger.effect.instances.dist._
//    implicit val ticker = Ticker()
//    checkAll("FunctorFilter[Dist]", FunctorFilterTests[DistT[IO, *]].functorFilter[Boolean, Boolean, Boolean])
//    checkAll("FunctorFilter[Dist]", SerializableTests.serializable(FunctorFilter[DistT[IO, *]]))
//  }

  checkAll(
    "Alternative[DistT]",
    AlternativeTests[DistT[Option, *]].alternative[Boolean, Boolean, Boolean])
  checkAll("Alternative[DistT]", SerializableTests.serializable(Alternative[DistT[Option, *]]))

}
