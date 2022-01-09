'use strict'
const $1 = ['Nil']
const $2 = {1: 2, 2: $1}
const $3 = ['Cons', $2]
const $4 = {1: 4, 2: $3}
const $5 = ['Cons', $4]
const cycle = xxs =>
  rec(cycle$1 => {
    const complete = xs => {
      if (xxs === xs) {
        return cycle$1
      } else {
        const [$6, $7] = xs
        if ($6 === 'Nil') {
          return cycle$1
        } else {
          return ['Cons', {1: $7[1], 2: complete($7[2])}]
        }
      }
    }
    return ['Cons', {1: xxs[1][1], 2: complete(xxs[1][2])}]
  })
cycle(cycle($5))
