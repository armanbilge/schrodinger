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

import cats.kernel.CommutativeMonoid

import scala.{specialized => sp}

/**
 * CommutativeBinoid represents a commutative binoid.
 *
 * A binoid is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CommutativeBinoid[@sp(Int, Long, Float, Double) A]
    extends Any
    with Binoid[A]
    with CommutativeMonoid[A]

object CommutativeBinoid extends BinoidFunctions[CommutativeBinoid] {

  /**
   * Access an implicit `CommutativeBinoid[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeBinoid[A]): CommutativeBinoid[A] = ev

  /**
   * Create a `CommutativeBinoid` instance from the given function and empty and absorbing values.
   */
  @inline def instance[A](
      emptyValue: A,
      absorbingValue: A,
      cmb: (A, A) => A): CommutativeBinoid[A] =
    new CommutativeBinoid[A] {
      override val empty: A = emptyValue
      override val absorbing: A = absorbingValue

      override def combine(x: A, y: A): A = cmb(x, y)
    }
}
