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

package schrodinger.generators

import cats.Id
import cats.data.StateT

trait Generator[S] {

  /**
   * Generates an
   */
  def nextInt: StateT[Id, S, Int]

  /**
   */
  def nextLong: StateT[Id, S, Long]

  /**
   * Generates a fair boolean.
   */
  def nextBoolean: StateT[Id, S, Boolean] =
    nextInt.map(_ >= 0)

  /**
   * Generates a uniform random float in [0.0, 1.0).
   */
  def nextFloat: StateT[Id, S, Float] =
    nextInt.map(x => (x >>> 8) * 5.9604645e-8f)

  /**
   * Generates a uniform random double in [0.0, 1.0).
   */
  def nextDouble: StateT[Id, S, Double] =
    nextLong.map(x => (x >>> 11) * 1.1102230246251565e-16)

  /**
   * Generates a uniform random double in [0.0, 1.0).
   */
  def nextGaussian: StateT[Id, S, Double]

  /**
   */
  def split: StateT[Id, S, S]

}

object Generator {

  @inline def apply[S](implicit generator: Generator[S]) = generator

}

trait IntBasedGenerator[S] extends Generator[S] {

  override def nextLong: StateT[Id, S, Long] = for {
    hi <- nextInt
    lo <- nextInt
  } yield (hi.toLong << 32) | lo.toLong

}

trait LongBasedGenerator[S] extends Generator[S] {

  override def nextInt: StateT[Id, S, Int] =
    nextLong.map(x => (x >>> 32).toInt)

}
