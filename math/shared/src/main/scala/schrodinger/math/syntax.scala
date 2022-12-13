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
  inline def +(y: A)(using A: AdditiveSemigroup[A]): A = A.plus(x, y)
  inline def *(y: A)(using A: MultiplicativeSemigroup[A]): A = A.times(x, y)
  inline def /(y: A)(using A: MultiplicativeGroup[A]): A = A.div(x, y)
  inline def reciprocal(using A: MultiplicativeGroup[A]): A = A.reciprocal(x)

extension [A](A: Rig[A])
  inline def fromInt(n: Int): A = fakeRing.fromInt(n)
  inline def fromBigInt(n: BigInt): A = fakeRing.fromBigInt(n)

  private def fakeRing: Ring[A] = new:
    def zero = A.zero
    def one = A.one
    def plus(x: A, y: A) = A.plus(x, y)
    def times(x: A, y: A) = A.times(x, y)
    def negate(x: A) = throw new UnsupportedOperationException
