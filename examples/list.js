'use strict'
const ᐟNone = ['None']
const ᐥFωμᐥ = 'Fωμ'
const 𝛍xsꓸᐟSome𛰙ᐥFωμᐥꓹ_xs𛰚 = rec(xs => ['Some', {1: ᐥFωμᐥ, 2: xs}])
const _takeN = n => xs => {
  if (n <= 0) {
    return ᐟNone
  } else {
    const [tag_xs, val_xs] = xs
    if (tag_xs === 'Some') {
      return ['Some', {1: val_xs[1], 2: _takeN((n - 1) | 0)(val_xs[2])}]
    } else {
      return ᐟNone
    }
  }
}
_takeN(5)(𝛍xsꓸᐟSome𛰙ᐥFωμᐥꓹ_xs𛰚)
