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

package schrodinger.data

import cats.kernel.CommutativeMonoid
import cats.kernel.instances.int.catsKernelStdOrderForInt
import cats.kernel.laws.discipline.{CommutativeMonoidTests, SerializableTests}
import cats.laws.discipline.arbitrary.catsLawsCogenForIor
import cats.laws.discipline.{AlignTests, CommutativeMonadTests, InvariantMonoidalTests}
import cats.{Align, CommutativeMonad, InvariantMonoidal}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.instances.int._
import schrodinger.testkit.weighted._

class WeightedSpec extends Specification with Discipline with ScalaCheck {

  checkAll(
    "CommutativeMonad[Weighted]",
    CommutativeMonadTests[Weighted[Int, *]].commutativeMonad[Int, Int, Int])
  checkAll(
    "CommutativeMonad[Weighted]",
    SerializableTests.serializable(CommutativeMonad[Weighted[Int, *]]))

  checkAll(
    "CommutativeMonoid[Weighted]",
    CommutativeMonoidTests[Weighted[Int, Int]].commutativeMonoid)
  checkAll(
    "CommutativeMonoid[Weighted]",
    SerializableTests.serializable(CommutativeMonoid[Weighted[Int, Int]]))

  checkAll(
    "InvariantMonoidal[Weighted]",
    InvariantMonoidalTests[Weighted[Int, *]].invariantMonoidal[Int, Int, Int])
  checkAll(
    "InvariantMonoidal[Weighted]",
    SerializableTests.serializable(InvariantMonoidal[Weighted[Int, *]]))

  checkAll("Align[Weighted]", AlignTests[Weighted[Int, *]].align[Int, Int, Int, Int])
  checkAll("Align[Weighted]", SerializableTests.serializable(Align[Weighted[Int, *]]))

}
