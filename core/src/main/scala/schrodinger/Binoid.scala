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

import cats.kernel.{Eq, Monoid, MonoidFunctions}

import scala.{specialized => sp}

/**
 * A binoid is a monoid with an absorbing element.
 * A binoid is a specialization of a monoid, so its operation must be associative.
 * Additionally, `combine(x, absorbing) == combine(absorbing, x) == absorbing`.
 * For example, if we have `Binoid[Double]`, with `combine` as multiplication, then `absorbing = 0`.
 *
 * @see https://arxiv.org/pdf/1603.02093.pdf
 */
trait Binoid[@sp(Int, Long, Float, Double) A] extends Any with Monoid[A] {

  /**
   * Return the absorbing element for this binoid.
   */
  def absorbing: A

  /**
   * Tests if `a` is the absorbing element.
   */
  def isAbsorbing(a: A)(implicit ev: Eq[A]): Boolean =
    ev.eqv(a, absorbing)

}

trait BinoidFunctions[B[T] <: Binoid[T]] extends MonoidFunctions[B] {
  def absorbing[@sp(Int, Long, Float, Double) A](implicit ev: B[A]): A =
    ev.absorbing

  def isAbsorbing[@sp(Int, Long, Float, Double) A](a: A)(implicit m: B[A], ev: Eq[A]): Boolean =
    m.isAbsorbing(a)
}

object Binoid extends BinoidFunctions[Binoid] {

  /**
   * Access an implicit `Binoid[A]`.
   */
  @inline final def apply[A](implicit ev: Binoid[A]): Binoid[A] = ev

  /**
   * Create a `Binoid` instance from the given function and empty and absorbing values.
   */
  @inline def instance[A](emptyValue: A, absorbingValue: A, cmb: (A, A) => A): Binoid[A] =
    new Binoid[A] {
      override val empty: A = emptyValue
      override val absorbing: A = absorbingValue

      override def combine(x: A, y: A): A = cmb(x, y)
    }
}
