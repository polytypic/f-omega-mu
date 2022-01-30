'use strict'
const $1 = ['None']
const $2 = 'Fωμ'
const $3 = rec(xs => ['Some', {1: $2, 2: xs}])
const takeN = n => xs => {
  if (n <= 0) {
    return $1
  } else {
    const [$4, $5] = xs
    if ($4 === 'Some') {
      return ['Some', {1: $5[1], 2: takeN((n - 1) | 0)($5[2])}]
    } else {
      return $1
    }
  }
}
takeN(5)($3)
