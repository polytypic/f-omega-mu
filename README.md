# Fωμ type checker &mdash; [![Build Status](https://travis-ci.org/polytypic/f-omega-mu.svg?branch=main)](https://travis-ci.org/polytypic/f-omega-mu)

A type checker for Fωμ roughly based on the article

<blockquote>
  <dl>
    <dt>
      <a href="https://dl.acm.org/doi/10.1145/2914770.2837660">
        System F-omega with Equirecursive Types for Datatype-generic Programming
      </a>
    </dt>
    <dd>Yufei Cai, Paolo G. Giarrusso, and Klaus Ostermann</dd>
  </dl>
</blockquote>

This is still very much Work-in-Progress with tons of missing features and
probably more bugs than one could imagine.

## Next steps

- Try the online playground:
  - [The traditional `fact`orial function](https://polytypic.github.io/f-omega-mu/#MQAgKgFgpiAuBOBDAJgS1qg9gO0QGxAAMAzRAY1kM3lXxGIFdsKtsAoNvKWe8ngXhCAe4FIUAXKmyxASYSTYAOjYgQgbuBsEqYuXLUxENhCATIhABGJdtjQDZ7SCh4AzjAMAqXhRAAKAwFpTAShBJNlEeAFYgA)
  - [Iterative `fib`onacci function](https://polytypic.github.io/f-omega-mu/#MQAgkgLgpgTghhAlgNyiABgM0QI3QewDs4BjExETAV0JKSICgGAbKCS3EAXgZBFfbN8+AA7cQgHuAhogFyJC7QEmEIeUpUKQy1QDpefEIG7gRHIXaDAKxMQzhK7v37EmEIRCATIhAAGPQ4gALKFdEH30oZgBnNGkxcxAACgoAahBzAEp41wBaEABGdPk9aK9c9SZsHFzPIA)
  - [Recursive `list` type encoding and `fold` function](https://polytypic.github.io/f-omega-mu/#MQAgSgpgxgrgTgZwJYDcIgAYBskIC4Yh4CeADuhAHZQD2AJkpQOYgCGldmAZjVnYVxjU8SGpQBQ4rBDxEy6HPhABeEIG7gPADpAPcCKtgACI4mgN6UkWAFwg4AGhC1KCK7MBJhCD0g3cAL6frIRklpWTMsFRBAbOAtCKM1KAtTcytbezEnIj8PV2tvTShNUICJYNTHcKjNNQALOgstDUsszRjK+MTLazsHdOysvx88vLSQGqIwwKkZEB4+cuijbRnagApsuH6ASn7M3Fk1yq5KOu2vSoAvC1iADycm8RAQG4BtOABdEHbws67h1TUruqVG4WJpLaaUEDLQ6PEBnLY3XwTEqkJAAfQYTCQeAQ4W6T0YeHeAGZIXiCe8AIyktL4yiEkAAFmpjlp9Kpy1CrNeGx5WwmSy5XMh-wsBMqTjFVxAAGoQAgtgAGEAo9FITHYoA)
  - [Generic `fold`](https://polytypic.github.io/f-omega-mu/#MQAg4gpgdhBOCWBjEADAZgewDYBMUCh8sIAXEEgTwAcIQAxAVykRI1hAF4RBu4DQC4AVICTCAQDpAAEQBDCQCNRACkkghIGQEplIeWhBKVO9SHhRCxMplyd8IEIGzgfsLG8AtpKp96TFmxBpR1mztpbkksAHMIGVhJDx09XX9AwMAe4BwMOmwcD2TtDRVpAKSbbgA3bNzEoqTQiKilbVcqAG0ctDUAXSbJdpA0jMsSjWNCCxwgA)
  - [List using a recursive sum type](https://polytypic.github.io/f-omega-mu/#MQAgMglgzgLiCuUIDsDmICGIBOBTAxvNkgG64hTwC2IMAngA64BQzANrnPUyG9HAF4QgbuAAFgBMAdIB7gGG0kBtZBDYAuEAG8AvgBoQ+APbIo6jRL1ytAXRApWHOMrYghgbOAYipy81aQ6vrC0NnbsnPpGUN7ukmLiqh7CcqoBXIqGxt5m4hZsvv78QbbI9mEoAGYoEDC4APJlUQkS8TJJKbSS6VAKMDYStM4hDrQYANa4AHINLaMT6ihwgEmEvAWLy4EezCAiyKrzMQAeJm0bW1sQ9cgggCZEIAAMtKK4xaenTt1Wm6e4bFAsL1uHfQYX6aT7-EBeITCAD6qm0kjePR0YP+nW8wnwcKatBy+XWWg6EXe+kkfQAFDAZpMycgALQARgAlCS5IyUVtfCFKWNxgpYNgUKgbABWEBk8qVap1PkwAVoGwAIgAYoBJ4GkCsZQA)
  - [Stack ADT using an existential type](https://polytypic.github.io/f-omega-mu/#MQAgygLghgxg1iAggEQCogK4GcCWA7AcxCjxAFMAPHLCMvCHKAGxAgE8AHMgKG6bIitOZEAHsODUaQC8IQN3AANwB0AbTxSyALhABvAL4AaEFlEBbLSAUBdEPj4ChXEE2qDZipYB7gFzVV4cTNr6RjBSWEEAFgAm2gpGEIHOrno2dvaC7E6QsAjuNDmaAFQggEmEIIVKOtwg5KYSbNqAAETK+fCWBtUgHNgRTcoKpcbQbQNlrQhxnRzifUrjloPikqQ6CswYFnFDBdsjetx6tni8-BCAUAzZ8AD044DQDNJnVTVkdewgsoDZwMpqAe+6h9ofIJrB0at0sBE-l8lHIKJplLDwkDLKpQngsH8dNE-hR4ixZBQsACkjRLFZQV1xFCEYTNMjlISQDAoFgRE8aiB-Pj5AB9TT6JSdDkgNQaTHEpY4KS6NZMDaxIzjQGuSwpCk1NEY9wAJ350QVrES9L0guFNRUJnMmNlGz+2qU0UVw1yIHtCQlEilKxtmydO2NVk6+z02kAwEQACkucAAlFcgbc0qdiDz5rIo0pwREVPgIDYAKwgSPOjM9bP0GwARkL6czZdzIAAzNXiy96nWrNHO0cTg4IKIecjPspPH2B65NPNRiTgTDwpPTeBi9MOCprMYmSy2ULOWL3HyBdvzVzxSBlaSQdvLSIdfyfQalbsJibDyLNZjsbJ7T68X9R8j7eMxL0oGNSHGkf6uO2ybjEAA)
- See the [syntax summary](SYNTAX.md).
- See project [TODO](TODO.md) list.

## Why?

[Typed λ-calculi](https://en.wikipedia.org/wiki/Typed_lambda_calculus), and
[System F](https://en.wikipedia.org/wiki/System_F) in particular, are popular
elaboration targets for programming languages. Here are a couple of papers using
such an approach:

<blockquote>
  <dl>
    <dt><a href="https://people.mpi-sws.org/~rossberg/1ml/">1ML — core and modules united</a></dt>
    <dd>Andreas Rossberg</dd>
  </dl>
  <dl>
    <dt><a href="https://arxiv.org/abs/1206.5386">Elaborating Intersection and Union Types</a>
    <dd>Jana Dunfield</dd>
  </dl>
</blockquote>

Perhaps a practical System Fωμ implementation could serve as a reusable building
block or as a forkable boilerplate when exploring such programming language
designs.

System Fω might also be a good language for teaching functional programming:

<blockquote>
  <dl>
    <dt><a href="https://dl.acm.org/doi/abs/10.1145/3342713">Lambda: the ultimate sublanguage (experience report)</a></dt>
    <dd>Jeremy Yallop, Leo White<br><a href="https://github.com/ocamllabs/fomega">System Fω interpreter for use in Advanced Functional Programming course</a><dd>
  </dl>
</blockquote>
