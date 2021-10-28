# Schrodinger

[![Latest version](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger/latest.svg?color=orange)](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger)

<img align="right" width="384px" src="https://user-images.githubusercontent.com/3119428/139197953-1abbf14c-0484-4a79-b6d0-927bd05a4546.png"/>

Schrodinger is an (early-stage) project for probabilistic programming in Scala 3, built for the [Cats](https://github.com/typelevel/cats) ecosystem. At its heart is the `RVT` monad transformer for modeling pseudo-random effects that is designed to fully compose with asynchronous effects such as `IO`. The goal is that this construct will facilitate the implementation of high-performance, concurrent Monte Carlo algorithms for simulation and inference. 

Furthermore, Schrodinger encodes the foundational probability distributions as a set of typeclasses to be used to write "universal" probabilistic programs, for which simulators and inference algorithms can be automatically derived by implementing interpreters in terms of these same typeclasses.

## Usage

```scala
libraryDependencies += "com.armanbilge" %% "schrodinger" % "0.3-193-ed9a8ba"
```

## Modules

* `kernel`: essential typeclasses for writing probabilistic programs.
  - `Random[F[_]]` encodes the primitive of generating random bits in the form of `Int` and `Long`.
  - `PseudoRandom[F[_], G[_], S]`: encodes the ability to pseudo-randomly simulate ("compile") a `Random` effect `F` to another effect `G` via a seed `S`. Extends `Random[F]`.
  - `Distribution[F[_], P, X]`: the Kleisli `P => F[X]`, encoding the mapping from parameters `P` (e.g., the mean and standard deviation of a Gaussian) for a distribution on `X` (e.g., the reals represented as `Double`) to an instance of an effect `F[X]` (e.g., an effect implementing the `Random` typeclass).
  - Various `case class`es parameterizing different distribution families, to be used as arguments for `P` above, as well as convenient aliases that can be used with the usual typeclass syntax.
  - `Density[F[_], X, R]`: the Kleisli `X => F[R]`, encoding the probability density (or probability mass) function in some effect `F`.
* `random`: samplers for the distribution families in `kernel`. These are implemented purely monadically, in terms of `Random[F]` or each other.
* `schrodinger`: the core module, also brings in `random`.
  - `RVT[F[_], S, A]`: a monad transformer for pseudo-random effects. Use this to simulate your probabilistic programs. Externally it is pure, although internally it is implemented with a mutable RNG for performance. Notably, it implements all the Cats Effect typeclasses up to `Async` (given that `F` is equally capable) by utilizing the underlying RNG's capability to deterministically ["split"](https://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html) itself, such that each fiber has its own source of randomness. Not only does this avoid contention and synchronization, it makes it possible to write pseudo-random programs that are concurrent yet _deterministic_, and thus reproducible. Anyone who has debugged a complex Monte Carlo algorithm knows this is a big deal.
  - `Rng[S]`: a mutable and thus unsafe random number generator with state `S`.
  - `RngDispatcher[F[_], S]`: "dispatches" a mutable RNG that can be used to run pseudo-random imperative programs, for interop with unsafe lands. This also relies on the splitting capability described above.
* `stats`: `Density` implementations for the distribution families in `kernel`. 
* `monte-carlo`: algorithms and datastructures for Monte Carlo inference.
  - `Weighted` and `WeightedT`: encodes a sample from a distribution along with its weight and probability density. This is useful for implementing importance sampling-based algorithms.
  - `ImportanceSampler`: derives a sampler for a distribution `P` in terms of a sampler for a distribution `Q`.
* `math`: assorted math stuff.
  - `LogDouble` for [probability calculations in log space](https://en.wikipedia.org/wiki/Log_probability).
* `laws`: currently empty besides a silly law for `PseudoRandom`. Still figuring this one out in [#2](https://github.com/armanbilge/schrodinger/issues/2).
* `kernel-testkit`: currently mostly used to test `random`.
  - the `PureRVT` monad, implemented in terms of Cats' `StateT`. It is completely pure, unlike `RVT` in core which is run with an unsafe mutable RNG.
  - \*waves hands\* `Eq` for a pseudo-random effect `F`.
* `testkit`: used to test `RVT`.

If not readily apparent, various aspects of the design are heavily influenced by [Cats Effect 3](https://github.com/typelevel/cats-effect).
* `monte-carlo` is like a "`std`" lib, and so-called middlewares can ideally be implemented only in terms of that and `kernel`. The implementations provided by the `random` and `stats` modules and `RVT` itself are only needed at runtime and indeed can be substituted with (more performant!) alternatives.
* The unsafe `Rng` that is used to simulate an `RVT` is kind of like the unsafe `IORuntime` that runs `IO`.
* `RngDispatcher` facilitates interop with "unsafe lands" inspired by the `Dispatcher` in CE3.
