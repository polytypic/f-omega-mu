# Fωμ type checker &mdash; Syntax summary

Below is an approximation of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
 kind : '(' kind ')'
      | '*'                                           // Type
      | kind '→' kind                                 // Type constructor

  typ : '(' typ ')'
      | tid                                           // Type variable (*1)
      | ('int' | 'bool' | 'string')                   // Builtin types
      | typ '→' typ                                   // Function type
      | '{' (label (':' typ)? ',')* '}'               // Product type
      | '[' (label (':' typ)? ',')* ']'               // Sum type
      | typ typ                                       // Apply type level function
      | 'λ' tid (':' kind)? '.' typ                   // Type level function
      | '∃' (tid (':' kind)? '.' typ | '(' typ ')')   // Existential type
      | '∀' (tid (':' kind)? '.' typ | '(' typ ')')   // Universal type
      | 'μ' (tid (':' kind)? '.' typ | '(' typ ')')   // Recursive type

  exp : '(' exp ')'
      | eid                                           // Variable (*1)
      | (int | 'true' | 'false' | string)             // Literals
      | '{' (label ('=' exp)? ',')* '}'               // Product introduction
      | exp '.' label                                 // Product elimination
      | '[' label ('=' exp)? ':' typ ']'              // Sum introduction
      | '<<' exp ':' typ '/' typ '>>'                 // Existential packing
      | exp exp                                       // Apply function
      | uop exp                                       // Apply unary operator
      | exp bop exp                                   // Apply binary operator
      | 'let' eid '=' exp 'in' exp                    // Binding
      | 'let' 'type' tid '=' typ 'in' exp             // Type binding (*2)
      | 'let' '<<' eid '/' tid '>>' '=' exp 'in' exp  // Existential unpacking (*3)
      | 'if' exp 'then' exp 'else' exp                // Conditional (*4)
      | 'λ' eid ':' typ '.' exp                       // Function
      | 'μ' (eid ':' typ '.' exp | '(' exp ')')       // Recursive expression (*5)
      | 'Λ' tid (':' kind)? '.' exp                   // Generalization
      | exp '[' typ ']'                               // Instantiation
      | 'target' '[' typ ']' string                   // Inline target (JavaScript) code

  uop : '¬'                                           // Logical negation
      | '+' | '-'                                     // Sign (*6)

  bop : '∨' | '∧'                                     // (L) Logical connectives (*7)
      | ('=' | '≠') '[' typ ']'                       // (-) Polymorphic equality
      | '>' | '≥' | '<' | '≤'                         // (-) Comparison
      | 'case'                                        // (L) Sum elimination
      | '+' | '-'                                     // (L) Additive
      | '*' | '/' | '%'                               // (L) Multiplicative
```

**Notes:**

- Binary operators are listed above in order from lowest to highest precedence
  and with associativity {`L`, `-`, `R`}.

- `#` begins comment to end-of-line.

1. Type variables are distinct from expression variables. `_` is allowed in
   place of a variable in bindings.

2. Bindings of types simply substitute the type into the body expression. System
   Fωμ does not have singleton kinds, for example.

3. Existential unpacking has the usual side condition of not allowing the type
   variable to escape.

4. This Fωμ implementation is strict.

5. Recursive expressions are currently not fully statically checked.

6. When applied to literals, sign operators are interpreted at compile-time.

7. Binary logical connectives, `∧` and `∨`, evaluate their arguments lazily.

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
|           `《` | `<<`                      |
|           `》` | `>>`                      |
