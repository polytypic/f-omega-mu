const help = {
  '{': `Record:

  {
    label: annotation = expression[,]
    label: annotation = expression[,]
    ...
  }`,
  '→': `Function:

  expression → expression`,
  '∀': `Universal quantification:

  ∀variable: kind.expression`,
  '∃': `Existential quantification:

  ∃variable: kind.expression`,
  '∧': `Conjunction:

  expression ∧ expression`,
  '∨': `Disjunction:

  expression ∨ expression`,
  '„': `Disjoint merge:

  expression „ expression`,
  case: `Case analyzing abstraction:

  case expression`,
  if: `Conditional:

  if expression then expression else expression`,
  import: `Import:

  import "path"`,
  include: `Include type definitions:

  include "path" [in]
  expression`,
  let: `Parallel definitions:

  let variable = value
  and variable = value ... [in]
  expression`,
  'let μ': `Recursive definitions:

  let μvariable = value
  and μvariable = value ... [in]
  expression`,
  local: `Local definitions:

  local definition [in]
  definitions`,
  type: `Parallel definition:

  type variable = expression
  and variable = expression ... [in]
  expression`,
  'type μ': `Recursive definitions:

  type μvariable = expression
  and μvariable = expression ... [in]
  expression`,
  Λ: `Generic expression:

  Λvariable: annotation.expression`,
  λ: `Abstraction:

  λpattern: annotation.expression`,
  μ: `Recursive expression:

  μvariable: annotation.expression`,
}
