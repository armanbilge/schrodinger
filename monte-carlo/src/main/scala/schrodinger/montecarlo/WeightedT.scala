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

package schrodinger.montecarlo

import algebra.ring.CommutativeRig
import algebra.ring.MultiplicativeMonoid
import algebra.ring.Rig
import algebra.ring.Semifield
import algebra.ring.Semiring
import cats.Alternative
import cats.Applicative
import cats.ApplicativeError
import cats.Apply
import cats.Contravariant
import cats.ContravariantMonoidal
import cats.Defer
import cats.Eq
import cats.Eval
import cats.FlatMap
import cats.Functor
import cats.Invariant
import cats.InvariantSemigroupal
import cats.Monad
import cats.Monoid
import cats.MonoidK
import cats.Order
import cats.Parallel
import cats.PartialOrder
import cats.Semigroup
import cats.SemigroupK
import cats.Show
import cats.kernel.Hash
import cats.syntax.InvariantSyntax
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.bitraverse.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.invariant.*
import cats.syntax.semigroup.*
import cats.~>
import schrodinger.kernel.Bernoulli
import schrodinger.kernel.Beta
import schrodinger.kernel.Categorical
import schrodinger.kernel.Dirichlet
import schrodinger.kernel.DiscreteUniform
import schrodinger.kernel.Exponential
import schrodinger.kernel.FairBernoulli
import schrodinger.kernel.Gamma
import schrodinger.kernel.Gaussian
import schrodinger.kernel.LogNormal
import schrodinger.kernel.Uniform
import schrodinger.math.syntax.*
import schrodinger.montecarlo.Weighted.Heavy
import schrodinger.montecarlo.Weighted.Weightless
import schrodinger.stats.Density

opaque type WeightedT[F[_], W, A] = F[Weighted[W, A]]
object WeightedT extends WeightedTInstances, WeightedTDistributionInstances:
  def apply[F[_], W, A](fwa: F[Weighted[W, A]]): WeightedT[F, W, A] = fwa

  def pure[F[_]: Applicative, W: MultiplicativeMonoid, A](a: A): WeightedT[F, W, A] =
    liftF(a.pure)

  def liftF[F[_]: Functor, W: MultiplicativeMonoid, A](fa: F[A]): WeightedT[F, W, A] =
    fa.map(Weighted.pure)

  def liftK[F[_]: Applicative, W: MultiplicativeMonoid]: F ~> WeightedT[F, W, _] =
    new F ~> WeightedT[F, W, _]:
      def apply[A](a: F[A]): WeightedT[F, W, A] = WeightedT.liftF(a)

  def liftFunctionK[F[_], G[_], A](f: F ~> G): WeightedT[F, A, _] ~> WeightedT[G, A, _] =
    new WeightedT[F, A, _] ~> WeightedT[G, A, _]:
      def apply[B](k: WeightedT[F, A, B]): WeightedT[G, A, B] = WeightedT.mapK(k)(f)

  def fromWeighted[F[_]: Applicative, W, A](wa: Weighted[W, A]): WeightedT[F, W, A] = wa.pure

  extension [F[_], W, A](wfa: WeightedT[F, W, A])

    def value: F[Weighted[W, A]] = wfa

    def imap[B](f: A => B)(g: B => A)(using F: Invariant[F]): WeightedT[F, W, B] =
      WeightedT(F.imap(value)(_.imap(f)(g))(_.imap(g)(f)))

    def mapK[G[_]](f: F ~> G): WeightedT[G, W, A] =
      WeightedT[G, W, A](f(value))

    def product[B](wfb: WeightedT[F, W, B])(
        using F: Applicative[F],
        W: Semiring[W],
        eq: Eq[W]): WeightedT[F, W, (A, B)] =
      F.map2(value, wfb.value)(_.product(_))

    def semiflatMap[B](f: A => F[B])(using F: Monad[F]): WeightedT[F, W, B] =
      F.flatMap(wfa) {
        case Weightless(w) => new Weightless(w).pure
        case Heavy(wa, da, a) => F.map(f(a))(Heavy(wa, da, _))
      }

    def semiflatTap[B](f: A => F[B])(using F: Monad[F]): WeightedT[F, W, A] =
      F.flatMap(wfa) {
        case weightless @ Weightless(_) => weightless.pure
        case heavy @ Heavy(_, _, a) => F.as(f(a), heavy)
      }

    def importance(
        f: A => W)(using F: Applicative[F], W0: Semifield[W], W1: Eq[W]): WeightedT[F, W, A] =
      WeightedT(F.map(value)(_.importance(f)))

    def importanceF(
        f: A => F[W])(using F: Monad[F], W0: Semifield[W], W1: Eq[W]): WeightedT[F, W, A] =
      F.flatMap(value)(_.importanceA(f))

    def show(using F: Show[F[Weighted[W, A]]]): String = F.show(value)

sealed private class WeightedTInstances extends WeightedTInstances0:

  given [F[_], W](using F: Defer[F]): Defer[WeightedT[F, W, _]] with
    def defer[A](fa: => WeightedT[F, W, A]): WeightedT[F, W, A] =
      WeightedT(F.defer(fa.value))

  given [F[_], W, A](using F: Order[F[Weighted[W, A]]]): Order[WeightedT[F, W, A]] =
    F.asInstanceOf[Order[WeightedT[F, W, A]]]

  given [F[_], W, A](using F: Show[F[Weighted[W, A]]]): Show[WeightedT[F, W, A]] =
    F.asInstanceOf[Show[WeightedT[F, W, A]]]

  given [F[_], W, A](using F: Monoid[F[Weighted[W, A]]]): Monoid[WeightedT[F, W, A]] =
    F.asInstanceOf[Monoid[WeightedT[F, W, A]]]

  given [F[_], W](
      using Applicative[F],
      Semiring[W],
      Eq[W]): InvariantSemigroupal[WeightedT[F, W, _]] =
    WeightedTInvariantSemigroupal[F, W]

sealed private class WeightedTInstances0 extends WeightedTInstances1:
  given [F[_], W, A](
      using F: PartialOrder[F[Weighted[W, A]]]): PartialOrder[WeightedT[F, W, A]] =
    F.asInstanceOf[PartialOrder[WeightedT[F, W, A]]]

  given [F[_], W, A](using F: Semigroup[F[Weighted[W, A]]]): Semigroup[WeightedT[F, W, A]] =
    F.asInstanceOf[Semigroup[WeightedT[F, W, A]]]

  given [F[_], W](using Invariant[F]): Invariant[WeightedT[F, W, _]] =
    WeightedTInvariant[F, W]

sealed private class WeightedTInstances1 extends WeightedTInstances2:
  given [F[_], W, A](using F: Hash[F[Weighted[W, A]]]): Hash[WeightedT[F, W, A]] =
    F.asInstanceOf[Hash[WeightedT[F, W, A]]]

sealed private class WeightedTInstances2:
  given [F[_], W, A](using F: Eq[F[Weighted[W, A]]]): Eq[WeightedT[F, W, A]] =
    F.asInstanceOf[Eq[WeightedT[F, W, A]]]

sealed private class WeightedTInvariant[F[_], W](using F: Invariant[F])
    extends Invariant[WeightedT[F, W, _]]:

  def imap[A, B](fa: WeightedT[F, W, A])(f: A => B)(g: B => A) =
    WeightedT.imap(fa)(f)(g)

sealed private class WeightedTInvariantSemigroupal[F[_], W](
    using F: Applicative[F],
    val W0: Semiring[W],
    val W1: Eq[W])
    extends WeightedTInvariant[F, W],
      InvariantSemigroupal[WeightedT[F, W, _]]:
  def product[A, B](fa: WeightedT[F, W, A], fb: WeightedT[F, W, B]): WeightedT[F, W, (A, B)] =
    WeightedT.product(fa)(fb)

sealed private trait WeightedTDistributionInstances:
  given [F[_]: FlatMap, W: MultiplicativeMonoid, B](
      using r: FairBernoulli[F, B],
      d: FairBernoulli[Density[F, W, _], B]): FairBernoulli[WeightedT[F, W, _], B] with
    def fairBernoulli = WeightedT {
      for
        b <- r.fairBernoulli
        d <- d.fairBernoulli(b)
      yield Weighted(d, b)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, P, B](
      using r: Bernoulli[F, P, B],
      d: Bernoulli[Density[F, W, _], P, B]): Bernoulli[WeightedT[F, W, _], P, B] with
    def bernoulli(successProbability: P) = WeightedT {
      for
        b <- r.bernoulli(successProbability)
        d <- d.bernoulli(successProbability)(b)
      yield Weighted(d, b)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, I](
      using r: DiscreteUniform[F, I],
      d: DiscreteUniform[Density[F, W, _], I]): DiscreteUniform[WeightedT[F, W, _], I] with
    def discreteUniform(n: I) = WeightedT {
      for
        i <- r.discreteUniform(n)
        d <- d.discreteUniform(n)(i)
      yield Weighted(d, i)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, V, I](
      using r: Categorical[F, V, I],
      d: Categorical[Density[F, W, _], V, I]): Categorical[WeightedT[F, W, _], V, I] with
    def categorical(probabilities: V) = WeightedT {
      for
        i <- r.categorical(probabilities)
        d <- d.categorical(probabilities)(i)
      yield Weighted(d, i)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Gaussian[F, A],
      d: Gaussian[Density[F, W, _], A]): Gaussian[WeightedT[F, W, _], A] with

    def gaussian = WeightedT {
      for
        x <- r.gaussian
        d <- d.gaussian(x)
      yield Weighted(d, x)
    }

    def gaussian(mean: A, standardDeviation: A) = WeightedT {
      for
        x <- r.gaussian(mean, standardDeviation)
        d <- d.gaussian(mean, standardDeviation)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: LogNormal[F, A],
      d: LogNormal[Density[F, W, _], A]): LogNormal[WeightedT[F, W, _], A] with

    def logNormal = WeightedT {
      for
        x <- r.logNormal
        d <- d.logNormal(x)
      yield Weighted(d, x)
    }

    def logNormal(mu: A, sigma: A) = WeightedT {
      for
        x <- r.logNormal(mu, sigma)
        d <- d.logNormal(mu, sigma)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Exponential[F, A],
      d: Exponential[Density[F, W, _], A]): Exponential[WeightedT[F, W, _], A] with

    def exponential = WeightedT {
      for
        x <- r.exponential
        d <- d.exponential(x)
      yield Weighted(d, x)
    }

    def exponential(rate: A) = WeightedT {
      for
        x <- r.exponential(rate)
        d <- d.exponential(rate)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Gamma[F, A],
      d: Gamma[Density[F, W, _], A]): Gamma[WeightedT[F, W, _], A] with
    def gamma(shape: A, rate: A) = WeightedT {
      for
        x <- r.gamma(shape, rate)
        d <- d.gamma(shape, rate)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Uniform[F, A],
      d: Uniform[Density[F, W, _], A]): Uniform[WeightedT[F, W, _], A] with

    def uniform01 = WeightedT {
      for
        x <- r.uniform01
        d <- d.uniform01(x)
      yield Weighted(d, x)
    }

    def uniform10 = WeightedT {
      for
        x <- r.uniform10
        d <- d.uniform10(x)
      yield Weighted(d, x)
    }

    def uniform(include: A, exclude: A) = WeightedT {
      for
        x <- r.uniform(include, exclude)
        d <- d.uniform(include, exclude)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Beta[F, A],
      d: Beta[Density[F, W, _], A]): Beta[WeightedT[F, W, _], A] with
    def beta(alpha: A, beta: A) = WeightedT {
      for
        x <- r.beta(alpha, beta)
        d <- d.beta(alpha, beta)(x)
      yield Weighted(d, x)
    }

  given [F[_]: FlatMap, W: MultiplicativeMonoid, A](
      using r: Dirichlet[F, A],
      d: Dirichlet[Density[F, W, _], A]): Dirichlet[WeightedT[F, W, _], A] with
    def dirichlet(concentration: A) = WeightedT {
      for
        x <- r.dirichlet(concentration)
        d <- d.dirichlet(concentration)(x)
      yield Weighted(d, x)
    }
