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

import algebra.instances.int.given
import cats.Alternative
import cats.ContravariantMonoidal
import cats.Defer
import cats.Eval
import cats.Order
import cats.Parallel
import cats.data.Const
import cats.effect.IO
import cats.effect.kernel.Async
import cats.effect.laws.AsyncTests
import cats.effect.testkit.TestInstances
import cats.kernel.Monoid
import cats.kernel.instances.finiteDuration.given
import cats.kernel.instances.int.catsKernelStdOrderForInt
import cats.kernel.instances.list.given
import cats.kernel.instances.option.given
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.DeferTests
import cats.laws.discipline.ParallelTests
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary.given
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

import scala.concurrent.duration.DurationInt

class WeightedTSpec
    extends Specification,
      Discipline,
      ScalaCheck,
      WeightedTestInstances,
      TestInstances:

  sequential

  given [A](using Order[Option[A]], Ticker): Order[WeightedT[IO, Int, A]] =
    Order.by { w =>
      unsafeRun(w.value).fold(
        None,
        _ => None,
        {
          case Some(Heavy(_, _, a)) => Some(a)
          case _ => None
        })
    }

  given [A](using Order[Option[A]]): Order[Weighted[Int, A]] =
    Order.by {
      case Heavy(_, _, a) => Some(a)
      case _ => None
    }

  given [F[_], A](using Order[F[Weighted[Int, A]]]): Order[WeightedT[F, Int, A]] =
    Order.by(WeightedT.value)

  given (using Ticker): Conversion[WeightedT[IO, Int, Boolean], Prop] = sbool =>
    ioBooleanToProp(WeightedT.value(sbool).map {
      case Heavy(_, _, a) => a
      case Weightless(_) => false
    })

  {
    given Ticker = Ticker()
    checkAll(
      "Async[WeightedT]",
      AsyncTests[WeightedT[IO, Int, _]].async[Int, Int, Int](10.millis))
  }
  checkAll("Async[WeightedT]", SerializableTests.serializable(Async[WeightedT[IO, Int, _]]))

  checkAll("Defer[WeightedT]", DeferTests[WeightedT[Eval, Int, _]].defer[Int])
  checkAll("Defer[WeightedT]", SerializableTests.serializable(Defer[WeightedT[Eval, Int, _]]))

  checkAll("Order[WeightedT]", OrderTests[WeightedT[Option, Int, Int]].order)
  checkAll(
    "Order[WeightedT]",
    SerializableTests.serializable(Order[WeightedT[Option, Int, Int]]))

  checkAll(
    "Parallel[WeightedT]",
    ParallelTests[WeightedT[Either[String, _], Int, _]].parallel[Int, Int])
  checkAll(
    "Parallel[WeightedT]",
    SerializableTests.serializable(Parallel[WeightedT[Either[String, _], Int, _]]))

  checkAll("Monoid[WeightedT]", MonoidTests[WeightedT[List, Int, Int]].monoid)
  checkAll(
    "Monoid[WeightedT]",
    SerializableTests.serializable(Monoid[WeightedT[List, Int, Int]]))

  checkAll(
    "Alternative[WeightedT]",
    AlternativeTests[WeightedT[List, Int, _]].alternative[Int, Int, Int])
  checkAll(
    "Alternative[WeightedT]",
    SerializableTests.serializable(Alternative[WeightedT[List, Int, _]]))

  checkAll(
    "ContravariantMonoidal[WeightedT]",
    ContravariantMonoidalTests[WeightedT[Const[Int, _], Int, _]]
      .contravariantMonoidal[Int, Int, Int])
  checkAll(
    "ContravariantMonoidal[WeightedT]",
    SerializableTests.serializable(ContravariantMonoidal[WeightedT[Const[Int, _], Int, _]]))
