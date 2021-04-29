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

package schrodinger.instances

import schrodinger.CommutativeGroup0

trait DoubleInstances {
  implicit val schrodingerStdGroup0ForDouble: CommutativeGroup0[Double] = new DoubleGroup0
}

class DoubleGroup0 extends CommutativeGroup0[Double] {
  override def absorbing: Double = 0.0
  override def combine(x: Double, y: Double): Double = x * y
  override def empty: Double = 1.0
  override def inverse(x: Double): Double = 1.0 / x
  override def remove(x: Double, y: Double): Double = x / y
}
