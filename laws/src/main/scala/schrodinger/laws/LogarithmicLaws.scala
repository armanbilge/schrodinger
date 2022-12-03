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

package schrodinger.laws

import schrodinger.math.Logarithmic
import cats.kernel.laws.*

trait LogarithmicLaws[A, L](using val L: Logarithmic[A, L]):

  def logarithmRoundTrip(a: A): IsEq[A] =
    L.exponential(L.logarithm(a)) <-> a

  def exponentialRoundTrip(l: L): IsEq[L] =
    L.logarithm(L.exponential(l)) <-> l

  def oneIsZero: IsEq[A] =
    L.exponential(L.semifield.one) <-> L.divisionRing.zero

  def timesIsPlus(x: L, y: L): IsEq[A] =
    L.exponential(L.semifield.times(x, y)) <->
      L.divisionRing.plus(L.exponential(x), L.exponential(y))

  def divIsMinus(x: L, y: L): IsEq[A] =
    L.exponential(L.semifield.div(x, y)) <->
      L.divisionRing.minus(L.exponential(x), L.exponential(y))

object LogarithmicLaws:
  def apply[A, L](using Logarithmic[A, L]): LogarithmicLaws[A, L] = new {}
  