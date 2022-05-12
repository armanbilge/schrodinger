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

package schrodinger.kernel

import cats.Functor
import cats.syntax.all.*

trait Uniform[F[_], A]:
  def uniform01: F[A]
  def uniform10: F[A]
  def uniform(include: A, exclude: A): F[A]

object Uniform:
  inline def apply[F[_], A](include: A, exclude: A)(using u: Uniform[F, A]): F[A] =
    u.uniform(include, exclude)

  inline def standard[F[_], A](using u: Uniform[F, A]): F[A] = u.uniform01

  given [F[_]: Functor: Random]: Uniform[F, Double] with

    def uniform01 = Random.long.map(x => (x >>> 11) * 1.1102230246251565e-16)

    def uniform10 = Random.long.map(x => ((x >>> 11) + 1) * 1.1102230246251565e-16)

    def uniform(include: Double, exclude: Double) =
      if exclude >= include then
        val delta = exclude - include
        uniform01.map(_ * delta + include)
      else
        val delta = include - exclude
        uniform10.map(_ * delta + exclude)
