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

package schrodinger.math

import algebra.ring.AdditiveCommutativeMonoid
import cats.kernel.PartialOrder

trait AdditiveCommutativeMonoidWithMonus[A] extends AdditiveCommutativeMonoid[A]:
  def monus(x: A, y: A): A

  def naturalOrder: PartialOrder[A]

  extension (x: A) final def ∸(y: A): A = monus(x, y)

object AdditiveCommutativeMonoidWithMonus:
  inline def apply[A](using A: AdditiveCommutativeMonoidWithMonus[A]): A.type = A
