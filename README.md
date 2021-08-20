# Schrodinger

[![Latest version](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger/latest.svg?color=orange)](https://index.scala-lang.org/armanbilge/schrodinger/schrodinger)

<img align="right" width="384px" style="padding: 100px" src="https://user-images.githubusercontent.com/3119428/116948367-09f10780-ac34-11eb-816a-8ffeea165d21.png"/>

Schrodinger is an (early-stage) project for probabilistic programming built for the [Cats](https://github.com/typelevel/cats) ecosystem.
At its heart is the `RandomT` monad transformer which enables you to safely inject randomness into your program by describing it as a random variable while also interleaving with other effects, such as [Cats Effect](https://github.com/typelevel/cats-effect)'s `IO` monad.
The goal is that this construct will facilitate the implementation of high-performance, concurrent Monte Carlo algorithms for simulation and inference.

Notably, via the use of "splittable" RNGs (versus a synchronized global or thread-locals), every random number generated can be deterministically traced back to the initial seed irrespective of execution order or thread assignment at runtime.
This substantially improves the reproducibility of concurrent programs.

## Usage

```scala
libraryDependencies += "com.armanbilge" %% "schrodinger" % "0.2.0"
// (optional) provides additional instances for cats-effect typeclasses
libraryDependencies += "com.armanbilge" %% "schrodinger-effect" % "0.2.0"
```

## Datatypes

* `schrodinger.RandomT`: a monad transformer for a random variable that describes a probabilistic program.
  It is a light wrapper for Cats' [`StateT`](https://typelevel.org/cats/datatypes/state.html) with its own typeclass instances.
  Hints:
    1. (Flat)Map the "primitive" random variables provided in `schrodinger.random.*` to create `RandomT` instances for your own datatypes.
    2. Use typeclasses to defer binding the RNG state type `S` to a specific implementation until the (terminal) simulation step.

  See this in action in the [example](#example-schrödingers-cat).
* `schrodinger.data.Weighted`: represents a "weighted" sample from a probability distribution.
  It is similar to Cats' [`Writer`](https://typelevel.org/cats/datatypes/writer.html) datatype with the additional feature that it short-circuits computation (like `Option`) if the weight goes to zero (see also [`Chronicle`](https://typelevel.org/cats-mtl/mtl-classes/chronicle.html) from Cats MTL).
  This is useful for (nested) [importance sampling](https://en.wikipedia.org/wiki/Importance_sampling) where samples from an auxiliary distribution are reweighted to sample from a target distribution.

## Example: Schrödinger's cat

In the [example project](https://github.com/armanbilge/schrodinger/blob/main/example/src/main/scala/schrodinger/ThoughtExperiment.scala) we reimagine the famous [thought experiment](https://en.wikipedia.org/wiki/Schr%C3%B6dinger%27s_cat) with the help of Cats Effect.
The experiment is modeled with three concurrent components:

* an atom that decays after a random exponentially-distributed time and is sensed by the Geiger counter
* a relay that is triggered by the Geiger counter to release poison and kill the cat :(
* an observer who starts the experiment, waits 1 second, and then checks on the cat

Although the only randomness in the program is the atom's decay, we build the entire program as a `RandomT[F[_], S, _]` and defer simulating it until "right before the end of the world" at which point we provide the initial seed for the RNG.
