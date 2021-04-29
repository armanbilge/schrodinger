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

import cats.kernel.Eq
import cats.kernel.laws.{IsEq, MonoidLaws}
import cats.kernel.laws.IsEqArrow

trait BinoidLaws[A] extends MonoidLaws[A] {
  implicit override def S: Binoid[A]

  def leftAbsorb(x: A): IsEq[A] =
    S.combine(S.absorbing, x) <-> S.absorbing

  def rightAbsorb(x: A): IsEq[A] =
    S.combine(x, S.absorbing) <-> S.absorbing

  def isAbsorbing(x: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(x, S.absorbing) <-> S.isAbsorbing(x)(eqv)
}

object BinoidLaws {
  def apply[A](implicit ev: Binoid[A]): BinoidLaws[A] =
    new BinoidLaws[A] { def S: Binoid[A] = ev }
}
