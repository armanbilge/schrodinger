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
import cats.Eval
import cats.kernel.instances.int.catsKernelStdOrderForInt
import cats.kernel.instances.list.*
import cats.kernel.instances.option.*
import cats.kernel.laws.discipline.MonoidTests
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.DeferTests
import cats.laws.discipline.InvariantSemigroupalTests
import cats.laws.discipline.arbitrary.*
import munit.DisciplineSuite

class WeightedTSuite extends DisciplineSuite {

  checkAll("WeightedT", DeferTests[WeightedT[Eval, Int, _]].defer[Int])
  checkAll("WeightedT", OrderTests[WeightedT[Option, Int, Int]].order)
  // checkAll("WeightedT", HashTests[WeightedT[Option, Int, Int]].hash)
  checkAll("WeightedT", MonoidTests[WeightedT[List, Int, Int]].monoid)
  checkAll(
    "WeightedT",
    InvariantSemigroupalTests[WeightedT[Eval, Int, _]].invariantSemigroupal[Int, Int, Int],
  )
}
