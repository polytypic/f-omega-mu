'use strict'
const $1 = ['Some', 101]
const $2 = cs => cs.Unit($1)
const $3 = ['Some', 42]
const $4 = cs => cs.Unit($3)
const $5 = cs => cs.Alt($2)($4)
const $6 = ['In2']
const $7 = v => $8 => v
const $8 = ['None']
const lookup = t =>
  t({
    Unit: $7,
    Alt: t1 => t2 => $10 => {
      const [$11, $12] = $10
      if ($11 === 'In2') {
        return lookup(t2)($12)
      } else {
        return lookup(t1)($12)
      }
    },
    Pair: t$1 => $9 => {
      const [$10, $11] = lookup(t$1)($9[1])
      if ($10 === 'Some') {
        return lookup($11)($9[2])
      } else {
        return $8
      }
    },
  })
lookup($5)($6)
