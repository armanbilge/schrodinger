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

package schrodinger.kernel.testkit

import cats.Eq
import cats.Monad
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.all.*
import org.apache.commons.math3.special.Gamma

final case class Confidence(replicates: Int, eqvThreshold: Double, neqvThreshold: Double)

class RandomEq[F[_]: Monad, A: Eq](
    using confidence: Confidence,
    exhaustive: ExhaustiveCheck[A],
    simulate: F[Boolean] => Boolean
) extends Eq[F[A]]:

  def eqv(x: F[A], y: F[A]): Boolean =
    val Confidence(replicates, eqvThreshold, neqvThreshold) = confidence
    import exhaustive.allValues

    val xs = x.replicateA(replicates)
    val ys = y.replicateA(replicates)

    val eqF = (xs, ys).mapN { (xs, ys) =>

      val xcounts = new Array[Int](allValues.size)
      val ycounts = new Array[Int](allValues.size)
      allValues.zipWithIndex.foreach { (a, i) =>
        xcounts(i) = xs.count(_ === a)
        ycounts(i) = ys.count(_ === a)
      }

      val p = equidistributedBelief(xcounts, ycounts, Array.fill(allValues.size)(1.0))

      if p > eqvThreshold then true
      else if (1 - p) > neqvThreshold then false
      else throw new EqUndecidableException
    }

    simulate(eqF)

  private def equidistributedBelief(
      trial1: Array[Int],
      trial2: Array[Int],
      dirichletPrior: Array[Double]): Double =
    val marginal1 = dirichletMultinomialLogPmf(trial1, trial2, dirichletPrior)
    val marginal2 = dirichletMultinomialLogPmf(trial1, dirichletPrior) +
      dirichletMultinomialLogPmf(trial2, dirichletPrior)
    math.exp(marginal1 - logPlus(marginal1, marginal2))

  private def dirichletMultinomialLogPmf(x: Array[Int], alpha: Array[Double]): Double =
    import Gamma.*
    val A = sum(alpha)
    val n = sum(x)
    var logPmf = logGamma(A) + logGamma(n + 1.0) - logGamma(n + A)
    var k = 0
    while k < x.length do
      logPmf += logGamma(x(k) + alpha(k)) - logGamma(alpha(k)) - logGamma(x(k) + 1.0)
      k += 1
    logPmf

  private def dirichletMultinomialLogPmf(
      x1: Array[Int],
      x2: Array[Int],
      alpha: Array[Double]): Double =
    import Gamma.*
    val A = sum(alpha)
    val n1 = sum(x1)
    val n2 = sum(x2)
    val n = n1 + n2
    var logPmf = logGamma(A) + logGamma(n1 + 1.0) + logGamma(n2 + 1.0) - logGamma(n + A)
    var k = 0
    while k < x1.length do
      logPmf += logGamma(x1(k) + x2(k) + alpha(k)) - logGamma(alpha(k)) - logGamma(
        x1(k) + 1.0) - logGamma(x2(k) + 1.0)
      k += 1
    logPmf

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

  private def logPlus(x: Double, y: Double): Double =
    import math.*
    if x < y then y + log1p(exp(x - y))
    else if x > y then x + log1p(exp(y - x))
    else // x == y
      log(2) + x
