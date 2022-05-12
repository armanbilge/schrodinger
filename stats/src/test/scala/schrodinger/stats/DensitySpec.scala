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

package schrodinger.stats

import algebra.instances.long.*
import cats.Eval
import cats.Functor
import cats.kernel.Eq
import cats.laws.discipline.InvariantMonoidalTests
import cats.laws.discipline.MiniInt
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline

class DensitySpec extends Specification, Discipline, ScalaCheck:

  given [F[_]: Functor, A](using arb: Arbitrary[A => F[Int]]): Arbitrary[Density[F, Long, A]] =
    Arbitrary(arb.arbitrary.map(_.andThen(_.map(_.toLong.abs))).map(Density(_)))

  given [F[_], P, A](using eq: Eq[A => F[P]]): Eq[Density[F, P, A]] =
    eq.imap(Density(_))(x => x)

  checkAll(
    "Density",
    InvariantMonoidalTests[Density[Eval, Long, _]].invariantMonoidal[MiniInt, MiniInt, MiniInt]
  )
