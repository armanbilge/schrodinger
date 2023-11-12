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

/** A type class which is a source of uniform random `Int`s and `Long`s.
  */
trait Random[F[_]] extends Serializable {
  def int: F[Int]
  def long: F[Long]
}

object Random {
  inline def int[F[_]](using F: Random[F]): F[Int] = F.int
  inline def long[F[_]](using F: Random[F]): F[Long] = F.long
}
