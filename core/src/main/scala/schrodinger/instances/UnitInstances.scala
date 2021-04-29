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

trait UnitInstances {
  implicit val schrodingerStdGroup0ForUnit: CommutativeGroup0[Unit] = new UnitGroup0
}

class UnitGroup0 extends CommutativeGroup0[Unit] {
  override def absorbing: Unit = ()
  override def combine(x: Unit, y: Unit): Unit = ()
  override def empty: Unit = ()
  override def inverse(x: Unit): Unit = ()
  override def remove(x: Unit, y: Unit): Unit = ()
}
