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

import algebra.instances.int.*
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
import cats.kernel.instances.finiteDuration.*
import cats.kernel.instances.int.catsKernelStdOrderForInt
import cats.kernel.instances.list.*
import cats.kernel.instances.option.*
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.AlternativeTests
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.DeferTests
import cats.laws.discipline.InvariantMonoidalTests
import cats.laws.discipline.InvariantSemigroupalTests
import cats.laws.discipline.ParallelTests
import cats.laws.discipline.arbitrary.*
import munit.DisciplineSuite
import org.scalacheck.Prop
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless

import scala.concurrent.duration.DurationInt

class WeightedTSuite extends DisciplineSuite:

  checkAll("WeightedT", DeferTests[WeightedT[Eval, Int, _]].defer[Int])
  checkAll("WeightedT", OrderTests[WeightedT[Option, Int, Int]].order)
  // checkAll("WeightedT", HashTests[WeightedT[Option, Int, Int]].hash)
  checkAll("WeightedT", MonoidTests[WeightedT[List, Int, Int]].monoid)
  checkAll(
    "WeightedT",
    InvariantSemigroupalTests[WeightedT[Eval, Int, _]].invariantSemigroupal[Int, Int, Int],
  )
