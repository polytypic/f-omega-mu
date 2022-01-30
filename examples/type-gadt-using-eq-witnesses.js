'use strict'
const $1 = x => x
const $2 = ['Bool', $1]
const $3 = ['String', $1]
const $4 = ['Int', $1]
const $5 = {1: $1, 2: $3, 3: $4}
const $6 = ['Alt', $5]
const $7 = {1: $1, 2: $2, 3: $6}
const $8 = ['Pair', $7]
const $9 = 'foo'
const $10 = ['In1', $9]
const $11 = {1: false, 2: $10}
const $12 = x => '' + x
const $13 = JSON.stringify
const $14 = "'In1("
const $15 = ')'
const $16 = "'In2("
const $17 = '('
const $18 = ', '
const to_string = $20 => {
  const [$21, $22] = $20
  switch ($21) {
    case 'String': {
      return x => $13($22($1)(x))
    }
    case 'Pair': {
      const to_string_a = to_string($22[2])
      const to_string_b = to_string($22[3])
      return t => {
        const $23 = $22[1]($1)(t)
        const s = to_string_b($23[2])
        return $17 + to_string_a($23[1]) + $18 + s + $15
      }
    }
    case 'Iso': {
      const to_string_b = to_string($22[3])
      return t => to_string_b($22[2][1]($22[1]($1)(t)))
    }
    case 'Alt': {
      const to_string_a = to_string($22[2])
      const to_string_b = to_string($22[3])
      return t => {
        const [$23, $24] = $22[1]($1)(t)
        if ($23 === 'In2') {
          return $16 + to_string_b($24) + $15
        } else {
          return $14 + to_string_a($24) + $15
        }
      }
    }
    default: {
      return x => $12($22($1)(x))
    }
  }
}
to_string($8)($11)
