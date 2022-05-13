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
package testkit

import cats.Eq
import cats.Monad
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.all.*
import org.apache.commons.math3.special.Gamma
import schrodinger.math.LogDouble
import scala.collection.mutable

final case class Confidence(replicates: Int, eqvThreshold: Double, neqvThreshold: Double)

object PseudoRandomEq:
  def apply[F[_]: Monad, G[_], S, A: Eq](
      using pseudo: PseudoRandom.Aux[F, G, S],
      seeds: ExhaustiveCheck[S],
      confidence: Confidence,
      eq: Eq[G[SimulationResult[A]]]
  ): Eq[F[A]] =
    import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
    Eq.by[F[A], S => G[SimulationResult[A]]](rv =>
      s => rv.replicateA(confidence.replicates).map(SimulationResult(_)).simulate(s))

final case class SimulationResult[A](samples: List[A])

object SimulationResult:
  given [A: Eq](using confidence: Confidence): Eq[SimulationResult[A]] =
    case (SimulationResult(xs), SimulationResult(ys)) =>
      import confidence.*

      val allValues = mutable.ArrayBuffer[A]()
      xs.foreach(a => if allValues.forall(_ =!= a) then allValues += a)
      ys.foreach(a => if allValues.forall(_ =!= a) then allValues += a)

      if allValues.size == 1 then true
      else
        val xcounts = new Array[Int](allValues.size)
        val ycounts = new Array[Int](allValues.size)
        allValues.zipWithIndex.foreach { (a, i) =>
          xcounts(i) = xs.count(_ === a)
          ycounts(i) = ys.count(_ === a)
        }

        val p = equidistributedBelief(xcounts, ycounts, Array.fill(allValues.size)(1.0))

        if p > eqvThreshold then true
        else if 1 - p > neqvThreshold then false
        else throw new EqUndecidableException

  private def equidistributedBelief(
      trial1: Array[Int],
      trial2: Array[Int],
      dirichletPrior: Array[Double]): Double =
    val marginal1 = dirichletMultinomialLogPmf(trial1, trial2, dirichletPrior)
    val marginal2 = dirichletMultinomialLogPmf(trial1, dirichletPrior) *
      dirichletMultinomialLogPmf(trial2, dirichletPrior)
    (marginal1 / (marginal1 + marginal2)).real

  private def gamma(x: Double): LogDouble =
    LogDouble.exp(Gamma.logGamma(x))

  private def dirichletMultinomialLogPmf(x: Array[Int], alpha: Array[Double]): LogDouble =
    val A = sum(alpha)
    val n = sum(x)
    var pmf = gamma(A) * gamma(n + 1.0) / gamma(n + A)
    var k = 0
    while k < x.length do
      pmf *= gamma(x(k) + alpha(k)) / gamma(alpha(k)) / gamma(x(k) + 1.0)
      k += 1
    pmf

  private def dirichletMultinomialLogPmf(
      x1: Array[Int],
      x2: Array[Int],
      alpha: Array[Double]): LogDouble =
    val A = sum(alpha)
    val n1 = sum(x1)
    val n2 = sum(x2)
    val n = n1 + n2
    var pmf = gamma(A) * gamma(n1 + 1.0) * gamma(n2 + 1.0) / gamma(n + A)
    var k = 0
    while k < x1.length do
      pmf *= gamma(x1(k) + x2(k) + alpha(k)) / gamma(alpha(k)) / gamma(x1(k) + 1.0) / gamma(
        x2(k) + 1.0)
      k += 1
    pmf

  private def sum(x: Array[Int]): Int =
    var i = 0
    var sum = 0
    while i < x.length do
      sum += x(i)
      i += 1
    sum

  private def sum(x: Array[Double]): Double =
    var i = 0
    var sum = 0.0
    while i < x.length do
      sum += x(i)
      i += 1
    sum
