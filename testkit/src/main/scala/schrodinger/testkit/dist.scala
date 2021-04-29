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

package schrodinger.testkit

import cats.laws.discipline.ExhaustiveCheck
import cats.{Applicative, Eq, FlatMap, Id, Monad}
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.traverse._
import org.apache.commons.math3.special.Gamma
import org.scalacheck.{Arbitrary, Gen}
import schrodinger.DistT
import schrodinger.distributions.{Bernoulli, Categorical}
import schrodinger.generators.Generator

object dist extends LowPriorityDistInstances {

  implicit def schrodingerTestKitEqForDistT[F[_], A, S](
      implicit ev0: Monad[F],
      ev1: Confidence,
      ev2: Discrete[A],
      ev3: Eq[F[A]],
      ev4: Generator[S],
      ev5: ExhaustiveCheck[S],
      ev6: F[Boolean] => Option[Boolean]): Eq[DistT[F, A]] =
    new DistTEq[F, A, S] {
      implicit override val F: Monad[F] = ev0
      implicit override val confidence: Confidence = ev1
      implicit override val discrete: Discrete[A] = ev2
      implicit override val eqFA: Eq[F[A]] = ev3
      implicit override val generator: Generator[S] = ev4
      implicit override val seeds: ExhaustiveCheck[S] = ev5
      implicit override val eval: F[Boolean] => Option[Boolean] = ev6
    }

  implicit def schrodingerTestKitArbitraryForDistT[F[_]: Applicative, A](
      implicit ev: Discrete[A]): Arbitrary[DistT[F, A]] =
    (ev.allValues, ev.dirichletPrior) match {
      case (List(a), _) => Arbitrary(Arbitrary.arbUnit.arbitrary.map(_ => DistT.pure(a)))
      case (List(a, b), List(1.0, 1.0)) =>
        Arbitrary(Gen.double.map(p => Bernoulli.applyF[F](p).map(if (_) a else b)))
      case (a, alpha) if alpha.forall(_ == 1.0) =>
        Arbitrary(
          for {
            p <- alpha.foldLeft(Arbitrary.arbUnit.arbitrary.map(_ => List.empty[Double])) {
              (list, _) =>
                for {
                  tail <- list
                  x <- Gen.exponential(1.0)
                } yield x :: tail
            }
          } yield Categorical.applyF(a.zip(p).toMap)
        )
      case _ =>
        ??? // TODO Sample probabilities from Dirichlet to create a categorical distribution
    }

  implicit val schrodingerTestKitDiscreteForUnit: Discrete[Unit] =
    Discrete.instance[Unit](List(()))

  implicit val schrodingerTestKitDiscreteForBoolean: Discrete[Boolean] =
    Discrete.instance(List(false, true))

  implicit def schrodingerTestKitEvalForId[A]: Id[A] => Option[A] =
    a => Some(a)

}

sealed trait LowPriorityDistInstances {
  implicit def schrodingerTestKitFallbackEqForDistT[
      F[_]: FlatMap,
      A,
      S: Generator: ExhaustiveCheck](implicit ev: Eq[F[A]]): Eq[DistT[F, A]] =
    DistTEq.fallback

  implicit def schrodingerTestKitFallbackArbitraryForDistT[F[_]: Applicative, A](
      implicit ev: Arbitrary[A]): Arbitrary[DistT[F, A]] =
    Arbitrary(ev.arbitrary.map(a => DistT.pure(a)))
}

final case class Confidence(replicates: Int, threshold: Double)

trait Discrete[A] extends ExhaustiveCheck[A] {
  def dirichletPrior: List[Double]
}

object Discrete {
  def apply[A](ev: Discrete[A]): Discrete[A] = ev

  def instance[A](values: List[A]): Discrete[A] =
    instance(values, values.map(_ => 1.0))

  def instance[A](values: List[A], prior: List[Double]): Discrete[A] =
    new Discrete[A] {
      override val allValues: List[A] = values
      override val dirichletPrior: List[Double] = prior
    }

  implicit def fromExhaustiveCheck[A](
      implicit exhaustiveCheck: ExhaustiveCheck[A]): Discrete[A] =
    instance(exhaustiveCheck.allValues)
}

object DistTEq {
  def fallback[F[_]: FlatMap, A, S: Generator: ExhaustiveCheck](
      implicit ev: Eq[F[A]]): Eq[DistT[F, A]] = {
    import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
    Eq.by[DistT[F, A], S => F[A]](d => s => d.sample(s))
  }
}

trait DistTEq[F[_], A, S] extends Eq[DistT[F, A]] {

  implicit def F: Monad[F]
  implicit def confidence: Confidence
  implicit def discrete: Discrete[A]
  implicit def eqFA: Eq[F[A]]
  implicit def generator: Generator[S]
  implicit def seeds: ExhaustiveCheck[S]
  implicit def eval: F[Boolean] => Option[Boolean]

  override def eqv(x: DistT[F, A], y: DistT[F, A]): Boolean = {

    val dirichletPrior = discrete.dirichletPrior.toArray
    val eqAtRequestedConfidence = for {
      trial1 <- countOutcomes(x)
      trial2 <- countOutcomes(y)
      // TODO Even when X === Y it is difficult to be certain of their equivalence
      // So unless we find evidence that X !== Y, we assume that X === Y
      // This works best if when X !== Y then they are very different
      // If X !== Y but they are similar, they may be falsely indicated as equivalent
      p = 1 - equidistributedBelief(trial1, trial2, dirichletPrior)
    } yield !(p > confidence.threshold)

    eval(seeds.allValues.forallM(eqAtRequestedConfidence.sample[S]))
      .getOrElse(DistTEq.fallback[F, A, S].eqv(x, y))
  }

  private def countOutcomes(fa: DistT[F, A]): DistT[F, Array[Int]] =
    Vector.fill(confidence.replicates)(fa).sequence.map { samples =>
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)
      discrete.allValues.map(counts.getOrElse(_, 0)).toArray
    }

  private def equidistributedBelief(
      trial1: Array[Int],
      trial2: Array[Int],
      dirichletPrior: Array[Double]): Double = {
    val marginal1 = dirichletMultinomialLogPmf(trial1, trial2, dirichletPrior)
    val marginal2 = dirichletMultinomialLogPmf(trial1, dirichletPrior) +
      dirichletMultinomialLogPmf(trial2, dirichletPrior)
    math.exp(marginal1 - logPlus(marginal1, marginal2))
  }

  private def dirichletMultinomialLogPmf(x: Array[Int], alpha: Array[Double]): Double = {
    import Gamma._
    val A = sum(alpha)
    val n = sum(x)
    var logPmf = logGamma(A) + logGamma(n + 1.0) - logGamma(n + A)
    var k = 0
    while (k < x.length) {
      logPmf += logGamma(x(k) + alpha(k)) - logGamma(alpha(k)) - logGamma(x(k) + 1.0)
      k += 1
    }
    logPmf
  }

  private def dirichletMultinomialLogPmf(
      x1: Array[Int],
      x2: Array[Int],
      alpha: Array[Double]): Double = {
    import Gamma._
    val A = sum(alpha)
    val n1 = sum(x1)
    val n2 = sum(x2)
    val n = n1 + n2
    var logPmf = logGamma(A) + logGamma(n1 + 1.0) + logGamma(n2 + 1.0) - logGamma(n + A)
    var k = 0
    while (k < x1.length) {
      logPmf += logGamma(x1(k) + x2(k) + alpha(k)) - logGamma(alpha(k)) - logGamma(
        x1(k) + 1.0) - logGamma(x2(k) + 1.0)
      k += 1
    }
    logPmf
  }

  private def sum(x: Array[Int]): Int = {
    var i = 0
    var sum = 0
    while (i < x.length) {
      sum += x(i)
      i += 1
    }
    sum
  }

  private def sum(x: Array[Double]): Double = {
    var i = 0
    var sum = 0.0
    while (i < x.length) {
      sum += x(i)
      i += 1
    }
    sum
  }

  private def logPlus(x: Double, y: Double): Double =
    if (x < y)
      y + math.log1p(math.exp(x - y))
    else if (x > y)
      x + math.log1p(math.exp(y - x))
    else // x == y
      math.log(2) + x

}
