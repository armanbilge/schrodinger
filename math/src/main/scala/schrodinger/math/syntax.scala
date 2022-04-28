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

package schrodinger.math.syntax

import algebra.ring.AdditiveSemigroup
import algebra.ring.MultiplicativeGroup
import algebra.ring.MultiplicativeSemigroup
import algebra.ring.Rig
import algebra.ring.Ring

extension [A](x: A)
  def +(y: A)(using A: AdditiveSemigroup[A]): A = A.plus(x, y)
  def *(y: A)(using A: MultiplicativeSemigroup[A]): A = A.times(x, y)
  def /(y: A)(using A: MultiplicativeGroup[A]): A = A.div(x, y)

extension (rig: Rig.type)
  def fromInt[A: Rig](n: Int): A = fakeRing.fromInt(n)
  def fromBigInt[A: Rig](n: BigInt): A = fakeRing.fromBigInt(n)

  private def fakeRing[A](using A: Rig[A]): Ring[A] = new:
    override def zero = A.zero
    override def one = A.one
    override def plus(x: A, y: A) = A.plus(x, y)
    override def times(x: A, y: A) = A.times(x, y)
    override def negate(x: A) = throw new UnsupportedOperationException
