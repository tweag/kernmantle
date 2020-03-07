# Kernmantle

Kernmantle braids some strands of effects into a `Rope` that will be the basis
of your application. By "effect", understand "any type representing a pure function or an impure (with side effects
in the general sense) computation
from an input value to an output value". The effects are interlaced, meaning they can be chained
like the following:

```haskell
eff1 >>> eff2 >>> eff1 >>> eff3 >>> eff1 >>> eff1 >>> eff2
```

where each effect can transmit its result to the next one.

This chain of interlaced effects is the `Rope`. Each effect is lifted in the
`Rope` with the `strand` function. Effects can be interpreted (transformed to an actual calculation, possibly with side-effects, or to other effect types) thanks to the `entwine`
function.

Each effect can represent some task in a pipeline. The way each task will be executed completely depends on the final interpretation functions given to `entwine`. That makes each task of the pipeline fully configurable, and each task or pipeline of tasks very easy to share and reuse. This also (as does any arrow pipeline) gives you the capacity to analyse the pipeline ahead-of-time, before it even runs.

More detailed information about the interest of this paradigm in the context of data science can be found in the [porcupine package documentation](https://tweag.github.io/porcupine/), which uses this paradigm.

## To build

Recommended way is to use stack.
Nix config might not be up to date.


## Why yet another effects library??

Existing effects library model only unary effects (effects parameterized only
by their result). Kernmantle models _binary_ effects:

```haskell
eff a b
```

where `eff` is most often contravariant in `a` (input) and covariant in `b`
(output) like any function (but the type system doesn't enforce that). The
simplest binary effect conceivable is therefore... the one that performs no
effect, that is, the pure function `(->)`.

## How do I create "binary effects"?

Currently, you have roughly four ways to create binary effects (more may exist,
please feel free to ping me if I forgot some):

- model them as (G)ADTs and write interpreter functions,
- compose them from regular applicative/monadic effects (`Reader`, `Writer`,
  `State` etc.),
- reuse existing binary effects (`Categories`, `Arrows`, `Profunctors` etc.),
- take the `Product` (from bifunctors) of 2 different binary effects. They are
  composed in parallel: results from one cannot be passed to the other.

The second option is particularly interesting given the huge amount of libraries
in Haskell exposing effects in the form of monads or applicative functors.

Using effects that implement some abstraction (like `Category`) isn't mandatory,
but the advantage of these effects is that you can compose several instances of
them before lifting them in the `Rope`.

### Composing regular applicative/monadic effects:

Binary effects can be of the form:

```haskell
type SomeBinEff f w m a b = f (w a -> m b)
```

If `f` is an `Applicative`, `w` a `Comonad`, `m` a `Monad`, and if `w` is
distributive over `m`, then `SomeBinEff f w m` will be a `Category` (and an
`Arrow`).

Any of `f`, `w` and `m` can just be `Identity`:

- If `f`, `w` and `m` are `Identity`, that's just a pure function `(->)`,
- If only `f` is non-Identity, that's the `Cayley` profunctor/category,
- If only `m` is non-Identity, that's the `Kleisli` category.
- If only `w` is non-Identity, that's the `Cokleisli` category,

Note that `f` can be composed out of several applicative functors (with
`Compose`). And of course `m` can use any usual monadic effect composition
(regular monad transformers, [capability](https://github.com/tweag/capability),
[polysemy](http://hackage.haskell.org/package/polysemy), etc). So reusability is
really key here.

To give an example, in the [porcupine](https://github.com/tweag/porcupine)
library which expresses data pipelines, `f` is a `Writer` effect that
accumulates the requirements of the pipeline (required files and resources,
parameters, etc.) and some execution context (namespace for the logger, etc.)
and `m` aggregates some logger, state and reader effects.

## The `Rope` type

The `Rope` type we introduce here allows to compose effects in a sequential
manner: they can pass results to one another. Branching is also supported (via
`ArrowChoice` or `Choice` classes), but fully dynamic determination of the
effect to run next depending on the result of a previous effect is precluded by
design (that would require `Monad`/`ArrowApply`).

`Rope` has the following signature:

```haskell
newtype Rope (record::RopeRec) (mantle::[Strand]) (core::BinEff) a b

-- The kinds used in Rope:
type BinEff = * -> * -> *
type Strand = (Symbol, BinEff) -- A strand is a bin effect with a name
type RopeRec = (Strand -> *) -> [Strand] -> * -- Collects all the methods to run each strand
```

It's parameterized over all the effects contained in the `Rope` (that's the
`mantle` part) and an extensible `record` (from
[`vinyl`](http://hackage.haskell.org/package/vinyl)) which will contain the way
to run the effects. Different `record` offer different trade-offs in terms of
extensibility (adding new effects, running existing effects, ie. interpreting
them in terms of other effects of the mantle or of the core) and algorithmic
complexity (when lifting an effect in the `Rope`).

The final parameter is the `core` effect in which the effects of the `mantle`
will end up being interpreted, but most of the code will maintain `core`
polymorphic and just add constraints to it.

Note that the `Rope` itself is a binary effect, so it can be reused in the
contexts we mentioned before.

## Do I need to use Arrows to use Kernmantle?

Categories are omnipresent in Haskell, but Arrows get a bad rap for some
reason. But effects in Kernmantle aren't required to instanciate any class. The
only requirement will be that in the end, you must be able to interpret them in
some core binary effect. `Rope` implements both the `Arrow` typeclass stack
(with the exception of `ArrowApply`, as we said) and the `Profunctor` stack, but
you don't have to use either one or the other to use Kernmantle. Effects can be
represented as pure (G)ADTs and you can manipulate them without resorting to any
other abstraction.


## Why the name?

A [Kernmantle rope](https://en.wikipedia.org/wiki/Kernmantle_rope) is a rope
composed of 2 parts, the core (kern) and the mantle. Like any rope, both parts
are made out of woven strands. The metaphor seemed quite nice for effects that
get interlaced, stranded together, and then interpreted in a _core_ effect.
