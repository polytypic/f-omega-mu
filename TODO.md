# Fωμ type checker &mdash; TODO

- Some known bugs:

  - Mutually recursive definitions (recursive products using destructuring) are
    not guaranteed to be translated correctly due to deconstructing the proxy
    object too early &mdash; the simplifier often "fixes" this issue by inlining
    the select operations

- Sandbox
  - highlight Fωμ based on grammar
- General
  - perform `let type ... in ...` substitutions in a single pass
  - better parser and type checker error messages
  - more testing
  - pretty printer for expressions
- Language
  - bidirectional type checking to reduce and generalize annotations
  - equality
  - existential types (missing proper error messages)
  - more flexible destructuring (placement of type annotations, partial
    patterns)
  - check recursive expressions for safety
  - more support for (JavaScript) primitives
    - bitwise operations on `int`
    - operations on `string`s
    - `BigInt`
    - `double`
    - `array` (as immutable?)
    - `println`
  - naïve modules/export
- Documentation
  - non-trivial examples
- ToJs:

  - type based simplifier
    - eliminate dead code when type is known to be uninhabited (e.g. empty sum)
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

    - more comprehensive constant folding

      - systematically rewrite commutative and associative expression to expose
        constant foldable subexpressions

    - rewrite simplify to be closer to linear time
    - specialize case analyzing function

      ```
      let f = fun x =>
        x case { C1 = ..., ... }
      ```

      to a set of case analyzing functions

      ```
      let F1 = fun v1 => ... in
      let ... in
      let f = fun x =>
        x case { C1 = F1, ... }
      ```

      to avoid allocating sum objects

  - code generation
    - [tail calls](https://stackoverflow.com/a/54721813)
    - hoist constant values (e.g. `rec`, `["nil": {}]}` -> `[nil, empty]`)
    - specialize recursive definitions
      - mutually recursive function definitions
      - product types
      - sum types
    - use destructuring e.g. for `λl.l case r` when `l` is not free in `r`
  - features
    - source maps
    - generate modules
