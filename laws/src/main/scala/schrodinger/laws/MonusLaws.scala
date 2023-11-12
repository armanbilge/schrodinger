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

package schrodinger.laws

import algebra.ring.AdditiveCommutativeMonoid
import cats.kernel.laws.*
import schrodinger.math.Monus
import schrodinger.math.syntax.*

trait MonusLaws[A](using val A: Monus[A]) {

  private given AdditiveCommutativeMonoid[A] = A.additiveCommutativeMonoid
  import given_AdditiveCommutativeMonoid_A.*

  def monusAxiom1(x: A, y: A) = x + (y ∸ x) <-> y + (x ∸ y)

  def monusAxiom2(x: A, y: A, z: A) =
    (x ∸ y) ∸ z <-> x ∸ (y + z)

  def monusAxiom3(x: A) = (x ∸ x) <-> zero

  def monusAxiom4(x: A) = (zero ∸ x) <-> zero

  def monusNaturalOrderConsistency(x: A, y: A) =
    A.naturalOrder.lteqv(x, x + y)
}

object MonusLaws {
  def apply[A: Monus]: MonusLaws[A] = new MonusLaws {}
}
