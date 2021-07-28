# Fωμ type checker &mdash; TODO

## Some known bugs

- Mutually recursive definitions (recursive products using destructuring) are
  not guaranteed to be translated correctly due to deconstructing the proxy
  object too early &mdash; the simplifier often "fixes" this issue by inlining
  the select operations

## Sandbox

- highlight Fωμ based on grammar production not just by lexical tokens

## General

- separate compilation improvements
  - optimize modules separately
- compilation server
  - keep checked and optimized modules in memory (only check if file changed)
- parallel IO during elaboration
- better parser and type checker error messages
- more testing
- pretty printer for expressions

## Language

- join and meet type expressions
- bidirectional type checking to reduce and generalize annotations
- equality
- more flexible destructuring (placement of type annotations, partial patterns)
- full pattern matching support
- check recursive expressions for safety
- more support for (JavaScript) primitives
  - bitwise operations on `int`
  - operations on `string`s
  - `BigInt`
  - `double`
  - `array` (as immutable?)
  - `println`

## Documentation

- non-trivial examples
- tutorial

## ToJs

- introduce AAST IL with full type information and make ToJS take that as input

- type based simplifier

  - eliminate dead code when type is known to be uninhabited (e.g. empty sum) or
    constant (e.g. Leibniz equality)

- simplifier

  - hoist `let` expressions

    ```
    if let x = 1 in ... then ... else ...
    ```

    out of subexpressions

    ```
    let x = 1 in
    if ... then ... else ...
    ```

    where possible

  - inline elimination form continuations systematically to expose redexes

  - common subexpression elimination

  - more comprehensive constant folding

    - systematically rewrite commutative and associative expression to expose
      constant foldable subexpressions

  - rewrite simplify to be closer to linear time

- code generation

  - [tail calls](https://stackoverflow.com/a/54721813)
  - hoist constant values (e.g. `rec`, `["nil": {}]}` -> `[nil, empty]`)
  - specialize recursive definitions
    - product types
    - sum types
  - use destructuring e.g. for `λl.l case r` when `l` is not free in `r`

- features
  - source maps
  - generate modules
