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

trait Exponential[F[_], R] {
  def standard: F[R]
  def apply(rate: R): F[R]
}

object Exponential {
  def apply[F[_], R](rate: R)(implicit exp: Exponential[F, R]): F[R] =
    exp(rate)

  def standard[F[_], R](implicit exp: Exponential[F, R]): F[R] =
    exp.standard
}