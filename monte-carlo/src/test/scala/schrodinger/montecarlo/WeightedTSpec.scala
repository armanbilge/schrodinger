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

package schrodinger.montecarlo

import cats.data.Const
import cats.effect.IO
import cats.effect.kernel.Async
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.kernel.Monoid
import cats.kernel.instances.finiteDuration._
import cats.kernel.instances.int.catsKernelStdOrderForInt
import cats.kernel.instances.list._
import cats.kernel.instances.option._
import cats.kernel.laws.discipline.{MonoidTests, OrderTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{
  AlternativeTests,
  ContravariantMonoidalTests,
  DeferTests,
  ParallelTests,
  SerializableTests
}
import cats.{Alternative, ContravariantMonoidal, Defer, Eval, Order, Parallel}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.montecarlo.Weighted.{Heavy, Weightless}

import scala.concurrent.duration.DurationInt

class WeightedTSpec
    extends Specification
    with Discipline
    with ScalaCheck
    with WeightedTestInstances
    with TestInstances {

  sequential

  implicit def weightedTIOOrder[A](
      implicit ev: Order[Option[A]],
      ticker: Ticker): Order[WeightedT[IO, Int, A]] =
    Order.by {
      case WeightedT(value) =>
        unsafeRun(value).fold(
          None,
          _ => None,
          {
            case Some(Heavy(_, a)) => Some(a)
            case _ => None
          })
    }

  implicit def weightedOrder[A](implicit ev: Order[Option[A]]): Order[Weighted[Int, A]] =
    Order.by {
      case Heavy(_, a) => Some(a)
      case _ => None
    }

  implicit def weightedTOrder[F[_], A](
      implicit ev: Order[F[Weighted[Int, A]]]): Order[WeightedT[F, Int, A]] =
    Order.by(_.value)

  implicit def execWeightedTIO(sbool: WeightedT[IO, Int, Boolean])(
      implicit ticker: Ticker): Prop =
    ioBooleanToProp(sbool.value.map {
      case Heavy(_, a) => a
      case Weightless(_) => false
    })

  {
    implicit val ticker = Ticker()
    checkAll(
      "Async[WeightedT]",
      AsyncTests[WeightedT[IO, Int, *]].async[Int, Int, Int](10.millis))
  }
  checkAll("Async[WeightedT]", SerializableTests.serializable(Async[WeightedT[IO, Int, *]]))

  checkAll("Defer[WeightedT]", DeferTests[WeightedT[Eval, Int, *]].defer[Int])
  checkAll("Defer[WeightedT]", SerializableTests.serializable(Defer[WeightedT[Eval, Int, *]]))

  checkAll("Order[WeightedT]", OrderTests[WeightedT[Option, Int, Int]].order)
  checkAll(
    "Order[WeightedT]",
    SerializableTests.serializable(Order[WeightedT[Option, Int, Int]]))

  checkAll(
    "Parallel[WeightedT]",
    ParallelTests[WeightedT[Either[String, *], Int, *]].parallel[Int, Int])
  checkAll(
    "Parallel[WeightedT]",
    SerializableTests.serializable(Parallel[WeightedT[Either[String, *], Int, *]]))

  checkAll("Monoid[WeightedT]", MonoidTests[WeightedT[List, Int, Int]].monoid)
  checkAll(
    "Monoid[WeightedT]",
    SerializableTests.serializable(Monoid[WeightedT[List, Int, Int]]))

  checkAll(
    "Alternative[WeightedT]",
    AlternativeTests[WeightedT[List, Int, *]].alternative[Int, Int, Int])
  checkAll(
    "Alternative[WeightedT]",
    SerializableTests.serializable(Alternative[WeightedT[List, Int, *]]))

  checkAll(
    "ContravariantMonoidal[WeightedT]",
    ContravariantMonoidalTests[WeightedT[Const[Int, *], Int, *]]
      .contravariantMonoidal[Int, Int, Int])
  checkAll(
    "ContravariantMonoidal[WeightedT]",
    SerializableTests.serializable(ContravariantMonoidal[WeightedT[Const[Int, *], Int, *]]))

}
