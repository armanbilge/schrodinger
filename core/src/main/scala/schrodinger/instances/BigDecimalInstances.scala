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

trait BigDecimalInstances {
  implicit val schrodingerStdGroup0ForBigDecimal: CommutativeGroup0[BigDecimal] =
    new BigDecimalGroup0
}

class BigDecimalGroup0 extends CommutativeGroup0[BigDecimal] {
  override def absorbing: BigDecimal = BigDecimal(0)
  override def combine(x: BigDecimal, y: BigDecimal): BigDecimal = x * y
  override def empty: BigDecimal = BigDecimal(1)
  override def inverse(x: BigDecimal): BigDecimal = 1 / x
  override def remove(x: BigDecimal, y: BigDecimal): BigDecimal = x / y
}
