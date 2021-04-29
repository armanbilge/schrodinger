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

import cats.kernel.{Group, MonoidFunctions}

import scala.{specialized => sp}

/**
 * A group with zero is a binoid where each element has an inverse or equivalently a group with an absorbing element.
 *
 * @see https://math.stackexchange.com/questions/1517237/is-there-a-name-for-a-monoid-with-a-distinguished-absorbing-element
 */
trait Group0[@sp(Int, Long, Float, Double) A] extends Any with Group[A] with Binoid[A]

// TODO Inherit from GroupFunctions when cats 2.6.1 is released
trait Group0Functions[G[T] <: Group0[T]] extends MonoidFunctions[G] with BinoidFunctions[G] {
  def inverse[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: G[A]): A =
    ev.inverse(a)

  def remove[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: G[A]): A =
    ev.remove(x, y)
}

object Group0 extends Group0Functions[Group0] {

  /**
   * Access an implicit `Group0[A]`.
   */
  @inline final def apply[A](implicit ev: Group0[A]): Group0[A] = ev

}
