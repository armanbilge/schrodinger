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

package schrodinger.random

import cats.Applicative
import cats.syntax.all.*
import schrodinger.kernel.Gamma
import schrodinger.kernel.Dirichlet

object dirichlet extends DirichletInstances

trait DirichletInstances:
  given schrodingerRandomDirichletForVectorDouble[F[_]: Applicative: Gamma[Double]]
      : Dirichlet[Vector[Double], Vector[Double]][F] =
    case Dirichlet.Params(concentration) =>
      concentration.traverse(Gamma(_, 1.0)).map { xs =>
        val sum = xs.sum
        xs.map(_ / sum)
      }
