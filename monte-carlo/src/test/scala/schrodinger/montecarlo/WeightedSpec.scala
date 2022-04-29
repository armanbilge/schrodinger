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
import cats.Align
import cats.CommutativeMonad
import cats.InvariantMonoidal
import cats.kernel.CommutativeMonoid
import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.laws.discipline.OrderTests
import cats.laws.discipline.AlignTests
import cats.laws.discipline.CommutativeMonadTests
import cats.laws.discipline.InvariantMonoidalTests
import cats.laws.discipline.arbitrary.*
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class WeightedSpec extends Specification, Discipline, ScalaCheck:

  checkAll("Weighted", CommutativeMonadTests[Weighted[Int, _]].commutativeMonad[Int, Int, Int])
  checkAll("Weighted", CommutativeMonoidTests[Weighted[Int, Int]].commutativeMonoid)
  checkAll(
    "Weighted",
    InvariantMonoidalTests[Weighted[Int, _]].invariantMonoidal[Int, Int, Int])
  checkAll("Weighted", AlignTests[Weighted[Int, _]].align[Int, Int, Int, Int])
  checkAll("Weighted", OrderTests[Weighted[Int, Int]].order)
