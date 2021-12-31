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
package random

import cats.Applicative
import cats.Traverse
import cats.syntax.all.*
import schrodinger.kernel.Categorical
import schrodinger.kernel.Multinomial
import schrodinger.math.LogDouble

import scala.collection.immutable.ArraySeq

object multinomial extends MultinomialInstances

trait MultinomialInstances:
  given [F[_]: Applicative: Categorical[G[P], Int], G[_]: Traverse, P]
      : Multinomial[G[P], Int, G[Int]][F] =
    case Multinomial.Params(support, trials) =>
      val categorical = Categorical(support)
      var acc = new Array[Int](support.size.toInt).pure
      var i = 0
      while i < trials do
        acc = (acc, categorical).mapN { (counts, i) =>
          counts(i) += 1
          counts
        }
        i += 1
      acc.map(counts => support.mapWithIndex((_, i) => counts(i)))

  given schrodingerRandomMultinomialForIArrayLogDouble[
      F[_]: Applicative: Categorical[IArray[LogDouble], Int]]
      : Multinomial[IArray[LogDouble], Int, IArray[Int]][F] with
    override def apply(params: Multinomial.Params[IArray[LogDouble], Int]): F[IArray[Int]] =
      import params.*
      val categorical = Categorical(support)
      var acc = new Array[Int](support.length).pure
      var i = 0
      while i < trials do
        acc = (acc, categorical).mapN { (counts, i) =>
          counts(i) += 1
          counts
        }
        i += 1
      acc.map(IArray.unsafeFromArray)
