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

import cats.kernel.CommutativeGroup

import scala.{specialized => sp}

/**
 * An commutative group-with-zero is a group
 * whose combine operation is commutative.
 */
trait CommutativeGroup0[@sp(Int, Long, Float, Double) A]
    extends Any
    with Group0[A]
    with CommutativeGroup[A]
    with CommutativeBinoid[A]

object CommutativeGroup0 extends Group0Functions[CommutativeGroup0] {

  /**
   * Access an implicit `CommutativeGroup0[A]`.
   */
  @inline final def apply[A](implicit ev: CommutativeGroup0[A]): CommutativeGroup0[A] = ev

}
