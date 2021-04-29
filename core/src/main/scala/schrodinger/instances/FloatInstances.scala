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

trait FloatInstances {
  implicit val schrodingerStdGroup0ForFloat: CommutativeGroup0[Float] = new FloatGroup0
}

class FloatGroup0 extends CommutativeGroup0[Float] {
  override def absorbing: Float = 0.0f
  override def combine(x: Float, y: Float): Float = x * y
  override def empty: Float = 1.0f
  override def inverse(x: Float): Float = 1.0f / x
  override def remove(x: Float, y: Float): Float = x / y
}
