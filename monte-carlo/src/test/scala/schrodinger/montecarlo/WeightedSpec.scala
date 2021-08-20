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

import cats.kernel.CommutativeMonoid
import cats.kernel.laws.discipline.{CommutativeMonoidTests, SerializableTests}
import cats.laws.discipline.arbitrary.given
import cats.laws.discipline.{AlignTests, CommutativeMonadTests, InvariantMonoidalTests}
import cats.{Align, CommutativeMonad, InvariantMonoidal}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class WeightedSpec
    extends Specification
    with Discipline
    with ScalaCheck
    with WeightedTestInstances:

  checkAll(
    "CommutativeMonad[Weighted]",
    CommutativeMonadTests[Weighted[Int, _]].commutativeMonad[Int, Int, Int])
  checkAll(
    "CommutativeMonad[Weighted]",
    SerializableTests.serializable(CommutativeMonad[Weighted[Int, _]]))

  checkAll(
    "CommutativeMonoid[Weighted]",
    CommutativeMonoidTests[Weighted[Int, Int]].commutativeMonoid)
  checkAll(
    "CommutativeMonoid[Weighted]",
    SerializableTests.serializable(CommutativeMonoid[Weighted[Int, Int]]))

  checkAll(
    "InvariantMonoidal[Weighted]",
    InvariantMonoidalTests[Weighted[Int, _]].invariantMonoidal[Int, Int, Int])
  checkAll(
    "InvariantMonoidal[Weighted]",
    SerializableTests.serializable(InvariantMonoidal[Weighted[Int, _]]))

  checkAll("Align[Weighted]", AlignTests[Weighted[Int, _]].align[Int, Int, Int, Int])
  checkAll("Align[Weighted]", SerializableTests.serializable(Align[Weighted[Int, _]]))
