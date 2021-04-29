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

import cats.kernel.laws.discipline.SerializableTests
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import schrodinger.instances.all._

class LawTests extends Specification with Discipline with ScalaCheck {

  checkAll("CommutativeGroup0[Unit]", CommutativeGroup0Tests[Unit].commutativeGroup0)
  checkAll("CommutativeGroup0[Unit]", SerializableTests.serializable(CommutativeGroup0[Unit]))
  checkAll("CommutativeBinoid[Boolean]", CommutativeBinoidTests[Boolean].commutativeBinoid)
  checkAll(
    "CommutativeBinoid[Boolean]",
    SerializableTests.serializable(CommutativeBinoid[Boolean]))
  checkAll("CommutativeBinoid[Byte]", CommutativeBinoidTests[Byte].commutativeBinoid)
  checkAll("CommutativeBinoid[Byte]", SerializableTests.serializable(CommutativeBinoid[Byte]))
  checkAll("CommutativeBinoid[Short]", CommutativeBinoidTests[Short].commutativeBinoid)
  checkAll("CommutativeBinoid[Short]", SerializableTests.serializable(CommutativeBinoid[Short]))
  checkAll("CommutativeBinoid[Int]", CommutativeBinoidTests[Int].commutativeBinoid)
  checkAll("CommutativeBinoid[Int]", SerializableTests.serializable(CommutativeBinoid[Int]))
  checkAll("CommutativeBinoid[Long]", CommutativeBinoidTests[Long].commutativeBinoid)
  checkAll("CommutativeBinoid[Long]", SerializableTests.serializable(CommutativeBinoid[Long]))
//  checkAll("CommutativeGroup0[Float]", CommutativeGroup0Tests[Float].commutativeGroup0) // approximately associative
  checkAll("CommutativeBinoid[Float]", SerializableTests.serializable(CommutativeBinoid[Float]))
//  checkAll("CommutativeGroup0[Double]", CommutativeGroup0Tests[Double].commutativeGroup0) // approximately associative
  checkAll(
    "CommutativeBinoid[Double]",
    SerializableTests.serializable(CommutativeBinoid[Double]))
  checkAll("CommutativeBinoid[BigInt]", CommutativeBinoidTests[BigInt].commutativeBinoid)
  checkAll(
    "CommutativeBinoid[BigInt]",
    SerializableTests.serializable(CommutativeBinoid[BigInt]))
//  checkAll("CommutativeGroup0[BigDecimal]", CommutativeGroup0Tests[BigDecimal].commutativeGroup0) // approximately associative
  checkAll(
    "CommutativeGroup0[BigDecimal]",
    SerializableTests.serializable(CommutativeGroup0[BigDecimal])
  ) // approximately associative

}
