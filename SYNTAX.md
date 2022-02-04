# Fωμ type checker &mdash; Syntax summary

Below is an _approximation_ of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
label
  : id | nat | string

kind
  : '(' kind ')'
  | '*'                                                   // Type
  | kind '→' kind                                         // Type constructor

typ
  : tid                                                   // Type variable (*1)
  | typ '→' typ                                           // Function type
  | typ '∨' typ                                           // Join of types (*2)
  | typ '∧' typ                                           // Meet of types (*2)
  | '(' (typ, ',')* ')'                                   // Tuple type
  | '{' (label (':' typ)?, ',')* '}'                      // Product type
  | '|'? ("'" label typ?, '|')*                           // Sum type
  | typ typ                                               // Apply type level function
  | 'λ' tid (':' kind)? '.' typ                           // Type level function
  | '∃' (tid (':' kind)? '.' typ | '(' typ ')')           // Existential type
  | '∀' (tid (':' kind)? '.' typ | '(' typ ')')           // Universal type
  | 'μ' (tid (':' kind)? '.' typ | '(' typ ')')           // Recursive type
  | typ_def 'in' typ                                      // Type bindings (*3)
  | 'import' string                                       // Import type

typ_def
  : 'type' (    tid (':' kind)? '=' typ, 'and')+          // Parallel type bindings
  | 'type' ('μ' tid (':' kind)? '=' typ, 'and')+          // Recursive type bindings
  | 'include' string                                      // Include type bindings

typ_defs
  : typ_def
  | typ_def 'in' typ_defs                                 // Sequential type binding
  | 'local' typ_def 'in' typ_defs                         // Local type binding

pat
  : eid                                                   // Variable pattern
  | '(' (pat, ',')* ')'                                   // Tuple pattern
  | '{' (label '=' pat, ',')* '}'                         // Product pattern
  | '«' tid ',' pat '»'                                   // Existential pack pattern

exp
  : '(' exp ')'
  | exp ':' typ                                           // Type ascription
  | eid                                                   // Variable (*1)
  | (nat | string)                                        // Literals
  | '(' (exp, ',')* ')'                                   // Tuple introduction
  | '{' (label ('=' exp)?, ',')* '}'                      // Product introduction
  | exp '.' (label | '(' exp ')')                         // Product elimination
  | "'" label exp?                                        // Sum introduction
  | 'case' exp                                            // Sum elimination (*4)
  | '«' typ ',' exp '»' ':' typ                           // Existential packing
  | exp exp                                               // Apply function
  | exp '◁' exp                                           // (R) Apply forward (*5)
  | exp '▷' exp                                           // (L) Apply backward (*5)
  | exp '◇' exp                                           // (L) Apply (*5)
  | uop exp                                               // Apply unary operator
  | exp bop exp                                           // Apply binary operator
  | typ_def 'in' exp                                      // Type bindings (*3)
  | 'let' (    pat (':' typ)? '=' exp, 'and')+ 'in' exp   // Parallel bindings (*6)
  | 'let' ('μ' pat  ':' typ   '=' exp, 'and')+ 'in' exp   // Recursive bindings (*6)
  | 'if' exp 'then' exp 'else' exp                        // Conditional (*7)
  | 'λ' pat ':' typ '.' exp                               // Function
  | 'μ' (pat ':' typ '.' exp)                             // Recursive expression (*8)
  | 'Λ' tid (':' kind)? '.' exp                           // Generalization
  | exp '[' typ ']'                                       // Instantiation
  | 'target' '[' typ ']' string                           // Inline JavaScript code
  | 'import' string                                       // Import value

uop
  : '¬'                                                   // Logical negation
  | '+' | '-'                                             // Sign (*9)

bop
  : '∨' | '∧'                                             // (L) Logical connectives (*10)
  | ('=' | '≠') '[' typ ']'                               // (-) Polymorphic equality
  | '>' | '≥' | '<' | '≤'                                 // (-) Comparison
  | '„'                                                   // (L) Merge (*11)
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

- To reduce noise, end of line commas (`,`) inside braces (`{ ... }`) and `in`
  keywords, and parentheses around binding constructs ( `Λ`, `λ`, `μ`, `∃`, and
  `∀` ) are automatically inserted based on layout.

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

3. Bindings of types simply substitute the type into the body expression. System
   Fωμ does not have singleton kinds, for example.

4. The expression given to `case` must be a record of functions corresponding to
   the sum to be eliminated. The result of `case` is a sum eliminating function.

5. `fₙ ◁ … ◁ f₁ ◁ x`, `x ▷ f₁ ▷ … ▷ fₙ`, and `f ◇ x₁ ◇ … ◇ xₙ` are special
   syntax for function application.

6. Type annotations must be specified in function parameters and recursive
   expressions and are optional in parallel `let` bindings. Existential
   unpacking has the usual side condition of not allowing the type variable to
   escape.

7. This Fωμ implementation is strict.

8. Recursive expressions are currently not fully statically checked.

9. When applied to literals, sign operators are interpreted at compile-time.

10. Binary logical connectives, `∧` and `∨`, evaluate their arguments lazily.

11. Disjoint merge `„` is allowed between nested disjoint products.

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
