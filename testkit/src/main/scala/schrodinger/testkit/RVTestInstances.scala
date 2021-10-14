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
package testkit

import cats.Applicative
import cats.Eq
import cats.Id
import cats.Monad
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.all.*
import org.apache.commons.math3.special.Gamma
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import schrodinger.RVT
import schrodinger.kernel.Bernoulli
import schrodinger.kernel.Categorical
import schrodinger.unsafe.rng.Rng
import schrodinger.unsafe.rng.SplitMix

trait RVTestInstances extends LowPriorityRVInstances:

  given schrodingerTestKitArbitraryForSplitMix: Arbitrary[SplitMix] = Arbitrary(
    for
      seed <- Arbitrary.arbLong.arbitrary
      gamma <- Arbitrary.arbLong.arbitrary
    yield SplitMix(seed, gamma)
  )

  given schrodingerTestKitEqForRVT[F[_], S, A](
      using Monad[F],
      Simulator[F],
      Rng[S],
      Confidence,
      Discrete[A],
      Eq[F[A]],
      ExhaustiveCheck[S],
      (F[Boolean] => Option[Boolean])): Eq[RVT[F, S, A]] =
    new RVTEq[F, A, S] {}

  given schrodingerTestKitCogenForRVT[F[_]: Simulator, S: Rng, A](
      using cogen: Cogen[F[A]],
      seeds: ExhaustiveCheck[S]): Cogen[RVT[F, S, A]] =
    cogen.contramap(rv => rv.simulate(seeds.allValues.head))

  given schrodingerTestKitArbitraryForRVT[F[_]: Monad, S, A](
      using ev: Discrete[A],
      b: Bernoulli[Double, Boolean][RVT[F, S, _]],
      c: Categorical[Seq[Double], Int][RVT[F, S, _]]): Arbitrary[RVT[F, S, A]] =
    (ev.allValues, ev.dirichletPrior) match
      case (List(a), _) => Arbitrary(Arbitrary.arbUnit.arbitrary.map(_ => RVT.pure(a)))
      case (List(a, b), List(1.0, 1.0)) =>
        Arbitrary(Gen.double.map(p => Bernoulli(p).map(if _ then a else b)))
      case (a, alpha) if alpha.forall(_ == 1.0) =>
        Arbitrary(
          for
            p <- alpha.foldLeft(Gen.const(List.empty[Double])) { (list, _) =>
              for
                tail <- list
                x <- Gen.exponential(1.0)
              yield x :: tail
            }
          yield Categorical(p).map(a)
        )
      case _ =>
        ??? // TODO Sample probabilities from Dirichlet to create a categorical distribution

  given schrodingerTestKitEvalForId[A]: (Id[A] => Option[A]) =
    a => Some(a)

sealed trait LowPriorityRVInstances:
  given schrodingerTestKitFallbackEqForRVT[F[_]: Simulator, A, S: Rng: ExhaustiveCheck](
      using Eq[F[A]]): Eq[RVT[F, S, A]] =
    RVTEq.fallback

  given schrodingerTestKitFallbackArbitraryForRVT[F[_]: Applicative, S, A](
      using ev: Arbitrary[A]): Arbitrary[RVT[F, S, A]] =
    Arbitrary(ev.arbitrary.map(a => RVT.pure(a)))

final case class Confidence(replicates: Int, threshold: Double)

object RVTEq:
  def fallback[F[_]: Simulator, S: Rng: ExhaustiveCheck, A](using Eq[F[A]]): Eq[RVT[F, S, A]] =
    import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
    Eq.by[RVT[F, S, A], S => F[A]](d => s => d.simulate(s))

trait RVTEq[F[_], A, S](
    using F: Monad[F],
    sim: Simulator[F],
    S: Rng[S],
    confidence: Confidence,
    discrete: Discrete[A],
    eqfa: Eq[F[A]],
    seeds: ExhaustiveCheck[S],
    eval: (F[Boolean] => Option[Boolean]))
    extends Eq[RVT[F, S, A]]:

  override def eqv(x: RVT[F, S, A], y: RVT[F, S, A]): Boolean =

    val dirichletPrior = discrete.dirichletPrior.toArray
    val eqAtRequestedConfidence = for
      trial1 <- countOutcomes(x)
      trial2 <- countOutcomes(y)
      // TODO Even when X === Y it is difficult to be certain of their equivalence
      // So unless we find evidence that X !== Y, we assume that X === Y
      // This works best if when X !== Y then they are very different
      // If X !== Y but they are similar, they may be falsely indicated as equivalent
      p = 1 - equidistributedBelief(trial1, trial2, dirichletPrior)
    yield !(p > confidence.threshold)

    eval(seeds.allValues.forallM(eqAtRequestedConfidence.simulate))
      .getOrElse(RVTEq.fallback[F, S, A].eqv(x, y))

  private def countOutcomes(fa: RVT[F, S, A]): RVT[F, S, Array[Int]] =
    Vector.fill(confidence.replicates)(fa).sequence.map { samples =>
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)
      discrete.allValues.map(counts.getOrElse(_, 0)).toArray
    }

  private def equidistributedBelief(
      trial1: Array[Int],
      trial2: Array[Int],
      dirichletPrior: Array[Double]): Double =
    val marginal1 = dirichletMultinomialLogPmf(trial1, trial2, dirichletPrior)
    val marginal2 = dirichletMultinomialLogPmf(trial1, dirichletPrior) +
      dirichletMultinomialLogPmf(trial2, dirichletPrior)
    scala.math.exp(marginal1 - logPlus(marginal1, marginal2))

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
    import scala.math.*
    if x < y then y + log1p(exp(x - y))
    else if x > y then x + log1p(exp(y - x))
    else // x == y
      log(2) + x
