# Fωμ type checker &mdash; Syntax summary

Below is an _approximation_ of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
label
  : id | nat | string

kind
  : '(' kind ')'
  | '_'                                                   // Infer kind
  | '*'                                                   // Type
  | kind '→' kind                                         // Type constructor

typ_bind
  : ('_' | tid) (':' kind)?                               // Type binding

typ
  : tid                                                   // Type variable (*1)
  | typ '→' typ                                           // Function type
  | typ '∨' typ                                           // Join of types (*2)
  | typ '∧' typ                                           // Meet of types (*2)
  | '(' (typ, ',')* ')'                                   // Tuple type (*3)
  | '{' (label (':' typ)?, ',')* '}'                      // Product type
  | '|'? ("'" label typ?, '|')*                           // Sum type
  | typ typ                                               // Apply type level function
  | 'λ' typ_bind '.' typ                                  // Type level function
  | '∃' (typ_bind '.' typ | '(' typ ')')                  // Existential type
  | '∀' (typ_bind '.' typ | '(' typ ')')                  // Universal type
  | 'μ' (typ_bind '.' typ | '(' typ ')')                  // Recursive type
  | typ_def 'in' typ                                      // Type bindings (*4)
  | 'import' string                                       // Import type

typ_def
  : 'type' (    typ_bind '=' typ, 'and')+                 // Parallel type bindings
  | 'type' ('μ' typ_bind '=' typ, 'and')+                 // Recursive type bindings
  | 'include' string                                      // Include type bindings

typ_defs
  : typ_def
  | typ_def 'in' typ_defs                                 // Sequential type binding
  | 'local' typ_def 'in' typ_defs                         // Local type binding

pat
  : eid                                                   // Variable pattern
  | '_'                                                   // Wildcard pattern
  | pat ':' typ                                           // Type annotation
  | '(' (pat, ',')* ')'                                   // Tuple pattern (*3)
  | '{' (label '=' pat, ',')* '}'                         // Product pattern
  | '«' typ_bind ',' pat '»'                              // Existential pack pattern

exp
  : '(' exp ')'
  | exp ':' typ                                           // Type annotation
  | eid                                                   // Variable (*1)
  | (nat | string)                                        // Literals
  | '(' (exp, ',')* ')'                                   // Tuple introduction (*3)
  | '{' (label ('=' exp)?, ',')* '}'                      // Product introduction
  | exp '.' (label | '(' exp ')')                         // Product elimination
  | "'" label exp?                                        // Sum introduction
  | 'case' exp                                            // Sum elimination (*5)
  | '«' typ ',' exp '»' ':' typ                           // Existential packing
  | exp exp                                               // Apply function
  | exp '◁' exp                                           // (R) Apply forward (*6)
  | exp '▷' exp                                           // (L) Apply backward (*6)
  | exp '◇' exp                                           // (L) Apply (*6)
  | uop exp                                               // Apply unary operator
  | exp bop exp                                           // Apply binary operator
  | typ_def 'in' exp                                      // Type bindings (*4)
  | 'let' (    pat '=' exp, 'and')+ 'in' exp              // Parallel bindings (*7)
  | 'let' ('μ' pat '=' exp, 'and')+ 'in' exp              // Recursive bindings (*7)
  | exp ';' exp                                           // Sequence (*8)
  | 'if' exp 'then' exp 'else' exp                        // Conditional
  | 'λ' pat '.' exp                                       // Function (*7)
  | 'μ' pat '.' exp                                       // Recursive expression (*7, *9)
  | 'Λ' typ_bind '.' exp                                  // Generalization
  | exp '[' typ ']'                                       // Instantiation
  | 'target' '[' typ ']' string                           // Inline JavaScript code
  | 'import' string                                       // Import value

uop
  : '¬'                                                   // Logical negation
  | '+' | '-'                                             // Sign (*10)

bop
  : '∨' | '∧'                                             // (L) Logical connectives (*11)
  | ('=' | '≠') '[' typ ']'                               // (-) Polymorphic equality
  | '>' | '≥' | '<' | '≤'                                 // (-) Comparison
  | '„'                                                   // (L) Merge (*12)
  | '+' | '-' | '^'                                       // (L) Additive
  | '*' | '/' | '%'                                       // (L) Multiplicative

mods                                                      // Syntax of .fom modules
  : exp eof

sigs                                                      // Syntax of .fomt signatures
  : typ eof

incs                                                      // Syntax of .fomd includes
  : typ_defs eof
```

**Notes:**

- To reduce noise, end of line commas (`,`) inside braces (`{ ... }`),
  semicolons (`;`), `in` keywords, and parentheses around binding constructs (
  `Λ`, `λ`, `μ`, `∃`, and `∀` ), conditionals ( `if … then … else …` ), pack
  expressions ( `«…, …»: …` ), and type annotations ( `: …` ) are automatically
  inserted based on layout.

- An identifier, right brace `}`, right bracket `]`, or right paren `)` followed
  without space by a left brace `{`, left bracket `[` or a left paren `(` is
  treated as a _high precedence instantiation, application, or introduction_.
  For example, `(o.f x).g y` can also be written as `o.f(x).g(y)`.

- Binary operators are listed above in order from lowest to highest precedence
  and with associativity {`L`, `-`, `R`}.

- `#` begins comment to end-of-line.

- `#line LINE "FILE"` is recognized as a line directive where `LINE` is a
  decimal line number and `"FILE"` is a JSON encoded file name that must be
  immediately followed by a newline. The following line will then be considered
  to come from the specified file and line.

- String literals are JSON strings with a couple of additions. See
  [example in sandbox](https://polytypic.github.io/f-omega-mu/#*examples/template-strings.fom)
  for details.

- The initial type environment has bindings for the builtin types

  - `bool: *`,
  - `int: *`, and
  - `string: *`

  and the initial value environment has bindings for the values

  - `false: bool`, and
  - `true: bool`

  and there are some other initially bound types and values that are for more
  esoteric uses.

- Underscore `_` is allowed in place of a variable in bindings. Also,
  identifiers starting with an underscore, e.g. `_not_used`, are not flagged as
  unused in the online editor.

1. Type variables are distinct from value variables. The lexical syntax of
   identifiers is similar to JavaScript except that `$` is not allowed in
   identifiers and that type variables allow some additional Unicode symbols.

2. Structural joins `∨` and meets `∧` are eliminated during type checking and
   are not allowed over arbitrary or unknown types.

3. For type checking side-effecting operations, the unit value and pattern `()`
   of unit type `()` is neither a tuple nor a product. There are no zero nor
   single element tuples.

4. Bindings of types simply substitute the type into the body expression. System
   Fωμ does not have singleton kinds, for example.

5. The expression given to `case` must be a record of functions corresponding to
   the sum to be eliminated. The result of `case` is a sum eliminating function.

6. `fₙ ◁ … ◁ f₁ ◁ x`, `x ▷ f₁ ▷ … ▷ fₙ`, and `f ◇ x₁ ◇ … ◇ xₙ` are special
   syntax for function application.

7. Sufficient type annotations must be specified in function parameters and
   recursive expressions and are optional in parallel `let` bindings.
   Existential unpacking has the usual side condition of not allowing the type
   variable to escape.

8. `exp₁ ; exp₂` is equivalent to `let () = exp₁ in exp₂`.

9. Recursive expressions are currently not fully statically checked.

10. When applied to literals, sign operators are interpreted at compile-time.

11. Binary logical connectives, `∧` and `∨`, evaluate their arguments lazily.

12. Disjoint merge `„` is allowed between nested disjoint products.

### Alternative tokens

| Unicode symbol | ASCII mnemonic            |
| -------------: | :------------------------ |
|            `.` | `=>`                      |
|            `«` | `<<`                      |
|            `¬` | `!`                       |
|            `»` | `>>`                      |
|            `Λ` | `gen`                     |
|            `λ` | `fun`                     |
|            `μ` | `rec`                     |
|            `„` | `,,`                      |
|            `→` | `->`                      |
|            `∀` | `forall`                  |
|            `∃` | `exists`                  |
|            `∧` | `&&`                      |
|            `∨` | <code>&#124;&#124;</code> |
|            `≠` | `!=`                      |
|            `≤` | `<=`                      |
|            `≥` | `>=`                      |
|            `▷` | <code>&#124;&gt;</code>   |
|            `◁` | <code>&lt;&#124;</code>   |
|            `◇` | `<>`                      |

Additionally, in the [online editor](https://polytypic.github.io/f-omega-mu/), a
backslash `\` followed by a symbol name, e.g. `\alpha`, is considered an escape
to be replaced with a unicode character, e.g. `α`, on space.
