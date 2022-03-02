'use strict'
const ᐟNil = ['Nil']
const ᐥFωμᐥ = 'Fωμ'
const 𝛍xsꓸᐟCons𛰙ᐥFωμᐥꓹ_xs𛰚 = rec(xs => ['Cons', {1: ᐥFωμᐥ, 2: xs}])
const _takeN = n => xs => {
  if (n <= 0) {
    return ᐟNil
  } else {
    const [tag_xs, val_xs] = xs
    if (tag_xs === 'Nil') {
      return ᐟNil
    } else {
      const $2$ = _takeN((n - 1) | 0)(val_xs[2])
      return ['Cons', {1: val_xs[1], 2: $2$}]
    }
  }
}
_takeN(5)(𝛍xsꓸᐟCons𛰙ᐥFωμᐥꓹ_xs𛰚)
