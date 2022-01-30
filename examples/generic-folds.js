'use strict'
const $1 = ['None']
const $2 = $1 => $2 => $1 === $2
const $3 = 'x'
const $4 = 'F'
const $5 = {id: $4}
const $6 = ['Var', $5]
const $7 = {id: $3}
const $8 = ['Var', $7]
const $9 = {fn: $6, arg: $8}
const $10 = ['App', $9]
const $11 = {var: $3, exp: $10}
const $12 = ['Abs', $11]
const $13 = 'y'
const $14 = {id: $13}
const $15 = ['Var', $14]
const $16 = {fn: $12, arg: $15}
const $17 = ['App', $16]
const has = p => $19 => {
  const [$20, $21] = $19
  if ($20 === 'Some') {
    return p($21[1]) || has(p)($21[2])
  } else {
    return false
  }
}
;(function fold($18) {
  const [$19, $20] = $18
  switch ($19) {
    case 'Var': {
      return ['Some', {1: $20.id, 2: $1}]
    }
    case 'Lit': {
      return $1
    }
    case 'App': {
      const fn = fold($20.fn)
      const arg = fold($20.arg)
      const ys = (function fold($21) {
        const [$22, $23] = $21
        if ($22 === 'Some') {
          const $2$ = fold($23[2])
          if (has($2($23[1]))(fn)) {
            return $2$
          } else {
            return ['Some', {1: $23[1], 2: $2$}]
          }
        } else {
          return $1
        }
      })(arg)
      return (function fold($21) {
        const [$22, $23] = $21
        if ($22 === 'Some') {
          const $2$ = fold($23[2])
          return ['Some', {1: $23[1], 2: $2$}]
        } else {
          return ys
        }
      })(fn)
    }
    default: {
      const exp = fold($20.exp)
      return (function fold($21) {
        const [$22, $23] = $21
        if ($22 === 'Some') {
          const $2$ = fold($23[2])
          if ($20.var === $23[1]) {
            return $2$
          } else {
            return ['Some', {1: $23[1], 2: $2$}]
          }
        } else {
          return $1
        }
      })(exp)
    }
  }
})($17)
