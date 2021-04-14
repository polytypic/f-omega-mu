# Fωμ type checker &mdash; [![Build Status](https://travis-ci.org/polytypic/f-omega-mu.svg?branch=main)](https://travis-ci.org/polytypic/f-omega-mu)

A type checker for Fωμ _restricted to non-nested types_.

This is basically a generalization of the type system for Fωμ\*, that is Fωμ
_restricted to first-order recursive types_ of kind `*`, described in the
article

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
μlist:* → *.λα:*.opt (α, list α)
```

needs to be rewritten as

```
λα:*.μlist_1:*.opt (α, list_1)
```

Both of the above types are allowed by this generalized system and are also
considered equivalent as shown in this
[example](https://polytypic.github.io/f-omega-mu/#MQAgMglgzgLiCmBHArhAbgQwDbwHYGN4AoInOGATwAd4QB7KuAXhEG7gQRuAAuAKgDoBtXHVzxOIABQBKADQgodALaiQ7ALogIuEmRCUaILNDggQLQD3Ah2DxCAkwhB8OPXgzjj2sy3HaSNWnXtpPAH0ARlM2Lj4LI1CnFwl3AxiQn01teDg6ADMgzxNwwGzgSN5WAA9OPJN2XlLfUgz6HOCwliKnMorklRq6+rh8AAt4fABrQuLWTzE86snk6a7qohNxbNyjfjUk2FDZNebN9U9UrSJB4ZGgA).

In this generalized system, nested types are not allowed. For example,

```
μnested:* → *.λα:*.(α, nested (α, α))
```

is disallowed due to the argument `(α, α)` as demonstrated in this
[example](https://polytypic.github.io/f-omega-mu/#MQAgcgpgzgLhAmJ4EMaoJ4AdomQJwiQEspkAbMgewHcEAoOsiGEGLQwHuAA7aOeALgBUIQEmEIYQF4QgbuBAjcBCAdAApZAGhA9YCECvWyAlPpBEuDJiy6UYAfXJVaiKdIAe-TX2NcYC554ZL9IA).

Disallowing nested types is sufficient to keep the number of distinct subtrees
finite in the infinite expansions of recursive types and to keep type
equivalence decidable.

In addition, this variation also has basic structural subtyping with joins (and
meets) and without bounded quantification. The motivation for providing
subtyping is to allow one to better exploit untyped compilation targets such as
JavaScript without having to e.g. perform unnecessary coercions.

This is still very much Work-in-Progress with tons of missing features and
probably more bugs than one could imagine.

## Next steps

- Try examples in the
  [online playground](https://polytypic.github.io/f-omega-mu/#MQAgKgFgpiAuBOBDAJgS1qg9gO0QGxAAMAzRAY1kM3lXxGIFdsKtsAoNvKWEQHuBSKALlTYegJMIQIngF42IEIG7gbMNEA6OfMnEQ2EIBMiEAEYN82NF3HNIKHgDOMXQCp65HgApdAWiMBKSezYBHgBWIA).
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
