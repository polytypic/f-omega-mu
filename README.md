# Fωμ type checker and compiler

A type checker and compiler
([\*](https://polytypic.github.io/f-omega-mu/#*examples/stream-fusion.fom)) for
Fωμ _restricted to non-nested types_
([\*](https://polytypic.github.io/f-omega-mu/#*examples/errors/nested.fom)).
This Fωμ variant has

- **structural sum and product types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/lists-of-various-lengths.fom),
  [\*](https://polytypic.github.io/f-omega-mu/#*examples/generic-folds.fom)),
- **equirecursive types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/first-order-and-higher-kinded-lists.fom),
  [\*](https://polytypic.github.io/f-omega-mu/#*examples/polymorphic-container-without-pretense.fom)),
- **higher-kinded types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/equality-witnesses.fom)),
  including **type level lambdas** and **kind inference**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/type-level-programming.fom)),
- **impredicative universal and existential types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/stack-adt.fom)),
- **structural subtyping**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/bounded-subtyping-of-counters.fom)),
- **decidable type checking**, and
- **phase separation**.

These features make Fωμ both relatively expressive
([\*](https://polytypic.github.io/f-omega-mu/#*examples/type-gadt-using-eq-witnesses.fom),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/hoas-gadt.fom)) and also
allow a compiler to make good use of untyped compilation targets
([\*](https://polytypic.github.io/f-omega-mu/#*examples/equirecursive-fixpoint-combinator.fom),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/object-oriented-sets.fom))
such as JavaScript.

The implementation also supports dividing a program into multiple files via an
`import` mechanism for types and values and an `include` mechanism for type
definitions. Value `import`s can be separately compiled. HTTPS URLs and relative
paths are allowed as references.

Please note that this is a hobby project and still very much Work-in-Progress
with tons of missing features and probably more bugs than one could imagine.

## Next steps

- Try examples in the
  [online playground](https://polytypic.github.io/f-omega-mu/#*examples/fact.fom).
- See the [syntax summary](SYNTAX.md).
- See [project board](https://github.com/polytypic/f-omega-mu/projects/1).

## Background

This Fωμ variant is basically _a generalization of_ the type system for Fωμ\*,
that is Fωμ _restricted to first-order recursive types_ of kind `*`, described
in the article

<blockquote>
  <dl>
    <dt>
      <a href="http://ps.informatik.uni-tuebingen.de/research/functors/equirecursion-fomega-popl16.pdf">
        System F-omega with Equirecursive Types for Datatype-generic Programming
      </a>
    </dt>
    <dd>by Yufei Cai, Paolo G. Giarrusso, and Klaus Ostermann.</dd>
  </dl>
</blockquote>

While Fωμ\* is powerful enough to express regular datatypes, it requires type
parameters to be hoisted outside of the `μ`. For example, the list type

```
μlist.λα.opt (α, list α)
```

needs to be rewritten as

```
λα.μlist_1.opt (α, list_1)
```

Both of the above types are allowed by this generalized system and are also
considered equivalent as shown in this
[example](https://polytypic.github.io/f-omega-mu/#*examples/first-order-and-higher-kinded-lists.fom).

In this generalized system, nested types are not allowed. For example,

```
μnested.λα.(α, nested (α, α))
```

is disallowed due to the argument `(α, α)` as demonstrated in this
[example](https://polytypic.github.io/f-omega-mu/#*examples/errors/nested.fom).

Disallowing nested types is sufficient to keep the number of distinct subtrees
finite in the infinite expansions of recursive types and to keep type
equivalence decidable.

In addition to higher-kinded equirecursive types, this variation also has kind
inference and basic structural subtyping with joins (and meets) and without
bounded quantification. The motivation for providing subtyping is to allow one
to better exploit untyped compilation targets such as JavaScript without having
to e.g. perform unnecessary coercions.

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
