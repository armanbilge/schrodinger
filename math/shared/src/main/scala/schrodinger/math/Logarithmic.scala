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

import algebra.ring.CommutativeSemifield
import algebra.ring.DivisionRing
import algebra.ring.Field
import algebra.ring.Semifield

trait Logarithmic[A, L]:

  def semifield: Semifield[L]

  def divisionRing: DivisionRing[A]

  def exponential(l: L): A

  def logarithm(a: A): L

  extension (l: L) inline def toExponential: A = exponential(l)

  extension (a: A) inline def toLogarithm: L = logarithm(a)

trait CommutativeLogarithmic[A, L] extends Logarithmic[A, L]:
  def commutativeSemifield: CommutativeSemifield[L]

  final def semifield = commutativeSemifield

  def field: Field[A]

  final def divisionRing = field

object Logarithmic:

  inline def apply[A, L](using l: Logarithmic[A, L]): l.type = l

  given CommutativeLogarithmic[Double, LogDouble] with
    def commutativeSemifield =
      LogDouble.given_CommutativeSemifield_LogDouble_Monus_LogDouble_Order_LogDouble_Hash_LogDouble

    def field = algebra.instances.all.doubleAlgebra

    def exponential(l: LogDouble) = l.log

    def logarithm(a: Double) = LogDouble.exp(a)
