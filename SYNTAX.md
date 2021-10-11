# Fωμ type checker &mdash; Syntax summary

Below is an _approximation_ of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
kind
  : '(' kind ')'
  | '*'                                                        // Type
  | kind '→' kind                                              // Type constructor

typ
  : tid                                                        // Type variable (*1)
  | typ '→' typ                                                // Function type
  | '(' (typ, ',')* ')'                                        // Tuple type
  | '{' (label (':' typ)?, ',')* '}'                           // Product type
  | '|' ("'" label typ?, '|')*                                 // Sum type
  | typ typ                                                    // Apply type level function
  | 'λ' tid (':' kind)? '.' typ                                // Type level function
  | '∃' (tid (':' kind)? '.' typ | '(' typ ')')                // Existential type
  | '∀' (tid (':' kind)? '.' typ | '(' typ ')')                // Universal type
  | 'μ' (tid (':' kind)? '.' typ | '(' typ ')')                // Recursive type
  | 'type' (    tid (':' kind)? '=' typ, 'and')+ 'in' typ      // Parallel type bindings (*4)
  | 'type' ('μ' tid (':' kind)? '=' typ, 'and')+ 'in' typ      // Recursive type bindings (*4)
  | 'include' string 'in' typ                                  // Include type definitions
  | 'import' string                                            // Import type

pat
  : eid                                                        // Variable pattern
  | '(' (pat, ',')* ')'                                        // Tuple pattern
  | '{' (label '=' pat, ',')* '}'                              // Product pattern
  | '<<' tid '\\' pat '>>'                                     // Existential pack pattern

exp
  : '(' exp ')'
  | exp ':' typ                                                // Type ascription
  | eid                                                        // Variable (*1)
  | (int | string)                                             // Literals
  | '(' (exp, ',')* ')'                                        // Tuple introduction
  | '{' (label ('=' exp)?, ',')* '}'                           // Product introduction
  | exp '.' (label | '(' exp ')')                              // Product elimination
  | "'" label exp?                                             // Sum introduction
  | 'case' exp                                                 // Sum elimination (*2)
  | '<<' typ '\\' exp '>>' ':' typ                             // Existential packing
  | exp exp                                                    // Apply function
  | exp '◁' exp                                                // (R) Apply forward (*3)
  | exp '▷' exp                                                // (L) Apply backward (*3)
  | exp '◇' exp                                                // (L) Apply (*3)
  | uop exp                                                    // Apply unary operator
  | exp bop exp                                                // Apply binary operator
  | 'type' (    tid (':' kind)? '=' typ, 'and')+ 'in' exp      // Parallel type bindings (*4)
  | 'type' ('μ' tid (':' kind)? '=' typ, 'and')+ 'in' exp      // Recursive type bindings (*4)
  | 'let' (    pat (':' typ)? '=' exp, 'and')+ 'in' exp        // Parallel bindings (*5)
  | 'let' ('μ' pat  ':' typ   '=' exp, 'and')+ 'in' exp        // Recursive bindings (*5)
  | 'if' exp 'then' exp 'else' exp                             // Conditional (*6)
  | 'λ' pat ':' typ '.' exp                                    // Function (*5)
  | 'μ' (pat ':' typ '.' exp | '(' exp ')')                    // Recursive expression (*7)
  | 'Λ' tid (':' kind)? '.' exp                                // Generalization
  | exp '[' typ ']'                                            // Instantiation
  | 'target' '[' typ ']' string                                // Inline target (JavaScript) code
  | 'include' string 'in' exp                                  // Include type definitions
  | 'import' string                                            // Import value

uop
  : '¬'                                                        // Logical negation
  | '+' | '-'                                                  // Sign (*8)

bop
  : '∨' | '∧'                                                  // (L) Logical connectives (*9)
  | ('=' | '≠') '[' typ ']'                                    // (-) Polymorphic equality
  | '>' | '≥' | '<' | '≤'                                      // (-) Comparison
  | '+' | '-' | '^'                                            // (L) Additive
  | '*' | '/' | '%'                                            // (L) Multiplicative
```

**Notes:**

- To reduce noise, end of line commas (`,`) inside braces (`{ ... }`) and `in`
  keywords, and parentheses around binding constructs ( `Λ`, `λ`, `μ`, `∃`, and
  `∀` ) are automatically inserted based on layout.

- An identifier, right bracket `]`, or right paren `)` followed without space by
  a left bracket `[` or a left paren `(` is treated as a _high precedence
  instantiation or application_, respectively. For example, `(o.f x).g y` can
  also be written as `o.f(x).g(y)`.

- Binary operators are listed above in order from lowest to highest precedence
  and with associativity {`L`, `-`, `R`}.

- `#` begins comment to end-of-line.

- `#line LINE "FILE"` is recognized as a line directive where `LINE` is a
  decimal line number and `"FILE"` is a JSON encoded file name that must be
  immediately followed by a newline. The following line will then be considered
  to come from the specified file and line.

- String literals are JSON strings with the addition of Standard ML style
  `\ws+\` ignored escape sequences where `ws+` is a non-empty sequence of
  whitespace allowing string literals to span multiple source lines.

1. Type variables are distinct from value variables.

   `_` is allowed in place of a variable in bindings.

   The initial type environment has bindings for the builtin types

   - `bool: *`,
   - `int: *`, and
   - `string: *`

   and the initial value environment has bindings for the values

   - `false: bool`, and
   - `true: bool`.

2. The expression given to `case` must be a record of functions corresponding to
   the sum to be eliminated. The result of `case` is a sum eliminating function.

3. `fₙ ◁ … ◁ f₁ ◁ x`, `x ▷ f₁ ▷ … ▷ fₙ`, and `f ◇ x₁ ◇ … ◇ xₙ` are special
   syntax for function application.

4. Bindings of types simply substitute the type into the body expression. System
   Fωμ does not have singleton kinds, for example.

5. Type annotations must be specified in function parameters and recursive
   expressions and are optional in parallel `let` bindings. Existential
   unpacking has the usual side condition of not allowing the type variable to
   escape.

6. This Fωμ implementation is strict.

7. Recursive expressions are currently not fully statically checked.

8. When applied to literals, sign operators are interpreted at compile-time.

9. Binary logical connectives, `∧` and `∨`, evaluate their arguments lazily.

### Alternative tokens

| Unicode symbol | ASCII mnemonic            |
| -------------: | :------------------------ |
|            `.` | `=>`                      |
|            `¬` | `!`                       |
|            `Λ` | `gen`                     |
|            `λ` | `fun`                     |
|            `μ` | `rec`                     |
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
|           `《` | `<<`                      |
|           `》` | `>>`                      |
