# Fωμ type checker &mdash; Syntax summary

Below is an _approximation_ of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
lab
  : eid | nat | string                            // Label

kind
  : '(' kind ')'
  | '_'                                           // Infer kind
  | '*'                                           // Type
  | kind '→' kind                                 // Type constructor

typ_pat
  : ('_' | tid) (':' kind)?                       // Type binding

typ_def
  : 'type' (    typ_pat '=' typ, 'and')+          // Parallel type bindings
  | 'type' ('μ' typ_pat '=' typ, 'and')+          // Recursive type bindings
  | 'include' string                              // Include type bindings

typ_defs
  : typ_def
  | typ_def 'in' typ_defs                         // Sequential type binding
  | 'local' typ_def 'in' typ_defs                 // Local type binding

typ
  : tid                                           // Type variable (*1)
  | typ ':' kind                                  // Kind annotation
  | typ '→' typ                                   // Function type
  | typ '∨' typ                                   // Join of types (*2)
  | typ '∧' typ                                   // Meet of types (*2)
  | '(' (typ, ',')* ')'                           // Tuple type (*3)
  | '{' (lab (':' typ)?, ',')* '}'                // Product type
  | '[' (typ, ',')* ('…' typ)? ']'                // Aggregate type
  | '|'? ("'" lab typ?, '|')*                     // Sum type
  | typ typ                                       // Apply type level function
  | 'λ' typ_pat '.' typ                           // Type level function
  | '∃' (typ_pat '.' typ | '(' typ ')')           // Existential type
  | '∀' (typ_pat '.' typ | '(' typ ')')           // Universal type
  | 'μ' (typ_pat '.' typ | '(' typ ')')           // Recursive type
  | typ_def 'in' typ                              // Type bindings (*4)
  | 'import' string                               // Import type

pat
  : eid                                           // Variable pattern
  | '_'                                           // Wildcard pattern
  | pat ':' typ                                   // Type annotation
  | '(' (pat, ',')* ')'                           // Tuple pattern (*3)
  | '{' (lab (':' typ)? ('=' pat)?, ',')* '}'     // Product pattern
  | "'" lab pat?                                  // Sum pattern
  | '«' typ_pat ',' pat '»'                       // Existential pack pattern

def
  : typ_def                                       // Type bindings (*4)
  | 'let' (    pat '=' exp, 'and')+               // Parallel bindings (*7)
  | 'let' ('μ' pat '=' exp, 'and')+               // Recursive bindings (*7)

exp
  : eid                                           // Variable (*1)
  | exp ':' typ                                   // Type annotation
  | (nat | string)                                // Literals
  | '(' (exp, ',')* ')'                           // Tuple introduction (*3)
  | '{' (lab (':' typ)? ('=' exp)?, ',')* '}'     // Product introduction
  | '[' (exp, ',')*  ('…' exp)? ']'               // Aggregate introduction
  | exp '.' (lab | '(' exp ')')                   // Product elimination
  | "'" lab exp?                                  // Sum introduction
  | 'case' exp                                    // Sum elimination (*5)
  | '«' typ ',' exp '»'                           // Existential packing
  | exp exp                                       // Apply function
  | exp '◁' exp                                   // (R) Apply forward (*6)
  | exp '▷' exp                                   // (L) Apply backward (*6)
  | exp '◇' exp                                   // (L) Apply (*6)
  | uop exp                                       // Apply unary operator
  | exp bop exp                                   // Apply binary operator
  | def 'in' exp                                  // Bindings
  | exp ';' exp                                   // Sequence (*8)
  | 'if' exp 'then' exp 'else' exp                // Conditional
  | 'λ' pat '.' exp                               // Function (*7)
  | 'μ' pat '.' exp                               // Recursive expression (*7, *9)
  | 'Λ' typ_pat '.' exp                           // Generalization
  | exp '«' typ '»'                               // Instantiation
  | 'target' '«' typ '»' string                   // Inline JavaScript code
  | 'import' string                               // Import value

uop
  : '¬'                                           // Logical negation
  | '+' | '-'                                     // Sign (*10)

bop
  : '∨' | '∧'                                     // (L) Logical connectives (*11)
  | ('=' | '≠') '«' typ '»'                       // (-) Polymorphic equality
  | '>' | '≥' | '<' | '≤'                         // (-) Comparison
  | '„'                                           // (L) Merge (*12)
  | '+' | '-' | '^'                               // (L) Additive
  | '*' | '/' | '%'                               // (L) Multiplicative

mods                                              // Syntax of .fom modules
  : exp eof

sigs                                              // Syntax of .fomt signatures
  : typ eof

incs                                              // Syntax of .fomd includes
  : typ_defs eof
```

**Notes:**

- An empty file is not valid syntax.

- To reduce noise, end of line commas (`,`) inside braces (`{ ... }`) and
  brackets (`[ ... ]`), semicolons (`;`), `in` keywords, and parentheses around
  binding constructs ( `Λ`, `λ`, `μ`, `∃`, and `∀` ), conditionals (
  `if … then … else …` ), and type annotations ( `: …` ) are automatically
  inserted based on layout.

- An identifier, right brace `}`, right bracket `]`', right double angle quote
  `»`, or right paren `)` followed without space by a left brace `{`, left
  bracket `[`, left double angle quote `«` or a left paren `(` is treated as a
  _high precedence instantiation, application, or introduction_. For example,
  `(o.f x).g y` can also be written as `o.f(x).g(y)`.

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

- String literals, natural numbers, and identifiers are allowed to serve as
  labels.

1. Type variables are distinct from value variables. The lexical syntax of
   identifiers is similar to JavaScript except that `$` is not allowed in
   identifiers and that type variables allow some additional Unicode symbols.

2. Structural joins `∨` and meets `∧` are eliminated during type checking and
   are not allowed over unrelated or unknown types.

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

9. Recursive expressions are currently not fully statically checked meaning that
   some obviously diverging expressions that should preferably be rejected are
   allowed.

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
