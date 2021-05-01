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

import schrodinger.util.Priority

import scala.collection.immutable.NumericRange

package object random {

  type PrioritizeGenerator[A] = Priority[A with GeneratorOverride, A]

  type FairBernoulli[F[_], S] = Bernoulli[F, S, Unit]

  type CategoricalInt[F[_], S] = Categorical[F, S, Seq[Double], Int]

  type ExponentialDouble[F[_], S] = Exponential[F, S, Double]

  type GaussianDouble[F[_], S] = Gaussian[F, S, Double]

  type UniformInt[F[_], S] = Uniform[F, S, Unit, Int]
  type UniformLong[F[_], S] = Uniform[F, S, Unit, Long]
  type UniformFloat[F[_], S] = Uniform[F, S, Unit, Float]
  type UniformDouble[F[_], S] = Uniform[F, S, Unit, Double]
  type UniformRange[F[_], S] = Uniform[F, S, Range, Int]
  type UniformLongRange[F[_], S] = Uniform[F, S, NumericRange[Long], Long]

}
