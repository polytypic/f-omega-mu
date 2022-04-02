# Fωμ type checker and compiler

A type checker and compiler
([\*](https://polytypic.github.io/f-omega-mu/#*examples/stream-fusion.fom 'Stream fusion'),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/template-strings.fom 'Template strings'))
for Fωμ _restricted to non-nested types_
([\*](https://polytypic.github.io/f-omega-mu/#*examples/errors/nested.fom 'Nested datatypes are disallowed')).
This Fωμ variant has

- **structural sum and product types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/lists-of-various-lengths.fom 'Types of lists of various lengths'),
  [\*](https://polytypic.github.io/f-omega-mu/#*examples/generic-folds.fom 'Generic folds')),
- **join and meet type operators**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/finally-tagless.fom 'Finally Tagless')),
- **equirecursive types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/first-order-and-higher-kinded-lists.fom 'Both first-order and higher-kinded recursive types are allowed'),
  [\*](https://polytypic.github.io/f-omega-mu/#*examples/polymorphic-container-without-pretense.fom 'Polymorphic container without pretense')),
- **higher-kinded types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/equality-witnesses.fom 'First-class type equality witnesses')),
  including **type level lambdas** and **kind inference**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/type-level-programming.fom 'Type level programming in Fωμ')),
- **impredicative universal and existential types**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/stack-adt.fom 'Stack ADT using an existential type')),
- **structural subtyping**
  ([\*](https://polytypic.github.io/f-omega-mu/#*examples/aggregate-syntax.fom 'Aggregate syntax'),
  [\*](https://polytypic.github.io/f-omega-mu/#*examples/bounded-subtyping-of-counters.fom 'Bounded subtyping of counters with identity coercions')),
- **decidable type checking**, and
- **phase separation**.

These features make Fωμ relatively well-behaved as well as expressive
([\*](https://polytypic.github.io/f-omega-mu/#*examples/type-gadt-using-eq-witnesses.fom 'GADT type encoding using type equality witnesses'),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/hoas-gadt.fom 'HOAS GADT using Scott encoding'),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/f-omega-self-interpreter.fom 'A self-interpreter for the Fω subset'))
and also allow a compiler to make good use of untyped compilation targets
([\*](https://polytypic.github.io/f-omega-mu/#*examples/equirecursive-fixpoint-combinator.fom 'Equirecursive applicative fixpoint combinator'),
[\*](https://polytypic.github.io/f-omega-mu/#*examples/object-oriented-sets.fom 'Object-oriented integer set implementations'))
such as JavaScript.

Although this is ultimately really intended to serve as an intermediate language
or elaboration target, the implementation provides both a fairly minimalistic
[AST](src/main/FomAST/FomAST.mli) and a somewhat more programmer friendly
[syntax](SYNTAX.md) with some convenience features that are elaborated into the
AST.

The implementation also supports dividing a program into multiple files via an
`import` mechanism for types and values and an `include` mechanism for type
definitions. Value `import`s can be separately compiled. HTTP URLs and relative
paths are allowed as references.

Please note that this is a hobby project and still very much Work-in-Progress
with tons of missing features, such as

- [partial type inference](https://github.com/polytypic/f-omega-mu/projects/1#card-65813070),
  and
- [variance or polarized types](https://github.com/polytypic/f-omega-mu/projects/1#card-74126731),

and probably more
[bugs](https://github.com/polytypic/f-omega-mu/projects/1#card-65813251) than
one could imagine.

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
[example](https://polytypic.github.io/f-omega-mu/#*examples/first-order-and-higher-kinded-lists.fom 'Both first-order and higher-kinded recursive types are allowed').

In this generalized system, nested types are not allowed. For example,

```
μnested.λα.(α, nested (α, α))
```

is disallowed due to the argument `(α, α)` as demonstrated in this
[example](https://polytypic.github.io/f-omega-mu/#*examples/errors/nested.fom 'Nested datatypes are disallowed').

Disallowing nested types is sufficient to keep the number of distinct subtrees
finite in the infinite expansions of recursive types and to keep type
equivalence decidable.

Fortunately, as conjectured in

<blockquote>
  <dl>
    <dt><a href="https://www.cis.upenn.edu/~plclub/blog/2020-12-04-nested-datatypes/">Do we need nested datatypes?</a></dt>
    <dd>by Stephanie Weirich<dd>
  </dl>
</blockquote>

many uses of nested datatypes can also be encoded using e.g. GADTs and Fωμ is
powerful enough to encode many GADTs. Consider, however, the higher-kinded
nested type

```
λf.μloop.λx.(x, loop (f x))
```

that, when given an `f` and an `x`, would expand to the infinite tree

```
(x,
 (f x,
  (f (f x),
   (f (f (f x)),
    ...))))
```

illustrating a process of unbounded computation where `x` could be higher-rank
and encode arbitrary data. It would seem that this kind of recursion pattern
would allow simulating arbitrary computations, which would seem to make type
equality undecidable. Thus, it would seem that disallowing nested types is not
only sufficient, but also necessary to keep type equivalence decidable in the
general case.

## Why?

Greg Morrisett has called Fω
["the workhorse of modern compilers"](https://web.archive.org/web/20140917015759/http://www.eecs.harvard.edu/~greg/cs256sp2005/lec16.txt).
Fωμ adds equirecursive types to Fω bringing it closer to practical programming
languages.

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

Here are a couple of papers about using a Fωμ variant as an IR:

<blockquote>
  <dl>
    <dt><a href="https://homepages.inf.ed.ac.uk/wadler/topics/iohk.html">Unraveling Recursion: Compiling an IR with Recursion to System F</a></dt>
    <dd>Michael Peyton Jones, Vasilis Gkoumas, Roman Kireev, Kenneth MacKenzie, Chad Nester, and Philip Wadler<dd>
  </dl>
  <dl>
    <dt><a href="https://homepages.inf.ed.ac.uk/wadler/topics/iohk.html">System F in Agda, for fun and profit</a></dt>
    <dd>James Chapman, Roman Kireev, Chad Nester, and Philip Wadler<dd>
  </dl>
</blockquote>

System Fω might also be a good language for teaching functional programming and
make an interesting object of study on its own:

<blockquote>
  <dl>
    <dt><a href="https://dl.acm.org/doi/abs/10.1145/3342713">Lambda: the ultimate sublanguage (experience report)</a></dt>
    <dd>Jeremy Yallop, Leo White<br><a href="https://github.com/ocamllabs/fomega">System Fω interpreter for use in Advanced Functional Programming course</a><dd>
  </dl>
  <dl>
    <dt><a href="https://dl.acm.org/doi/abs/10.1145/2837614.2837623">Breaking Through the Normalization Barrier: A Self-Interpreter for F-omega</a></dt>
    <dd>Matt Brown, Jens Palsberg<dd>
  </dl>
</blockquote>
