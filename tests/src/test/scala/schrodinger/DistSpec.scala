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

import algebra.instances.all.*
import cats.CommutativeMonad
import cats.laws.discipline.CommutativeMonadTests
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class DistSpec extends Specification, Discipline:

  given [A: Arbitrary]: Arbitrary[Dist[Int, A]] =
    Arbitrary(
      Gen
        .nonEmptyListOf(for
          a <- Arbitrary.arbitrary[A]
          p <- Gen.geometric(100)
        yield a -> p)
        .map(_.toMap)
        .map(Dist(_)))

  given CommutativeMonad[Dist[Int, *]] = Dist.commutativeMonad[Int](1024)

  checkAll(
    "Dist",
    CommutativeMonadTests[Dist[Int, _]].commutativeMonad[Boolean, Boolean, Boolean]
  )
