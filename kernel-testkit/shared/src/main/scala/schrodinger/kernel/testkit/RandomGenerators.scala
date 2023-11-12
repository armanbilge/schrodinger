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

package schrodinger.kernel
package testkit

import cats.Functor
import cats.effect.kernel.testkit.Generators1
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait RandomGenerators[F[_]: Random] extends Generators1[F] {
  implicit val F: Functor[F]

  override protected def baseGen[A: Arbitrary: Cogen]: List[(String, Gen[F[A]])] =
    List("randomInt" -> genRandomInt[A], "randomLong" -> genRandomLong[A]) ++ super.baseGen[A]

  private def genRandomInt[A: Arbitrary]: Gen[F[A]] =
    for f <- Arbitrary.arbitrary[Int => A]
    yield Random.int.map(f)

  private def genRandomLong[A: Arbitrary]: Gen[F[A]] =
    for f <- Arbitrary.arbitrary[Long => A]
    yield Random.long.map(f)
}
