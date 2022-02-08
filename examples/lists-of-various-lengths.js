'use strict'
const ᐟNil = ['Nil']
const 𛰙2ꓹ_ᐟNil𛰚 = {1: 2, 2: ᐟNil}
const ᐟCons𛰙2ꓹ_ᐟNil𛰚 = ['Cons', 𛰙2ꓹ_ᐟNil𛰚]
const 𛰙4ꓹ_ᐟCons𛰙2ꓹ_ᐟNil𛰚𛰚 = {1: 4, 2: ᐟCons𛰙2ꓹ_ᐟNil𛰚}
const ᐟCons𛰙4ꓹ_ᐟCons𛰙2ꓹ_ᐟNil𛰚𛰚 = ['Cons', 𛰙4ꓹ_ᐟCons𛰙2ꓹ_ᐟNil𛰚𛰚]
const cycle = xxs =>
  rec(cycle$1 => {
    const _complete = xs => {
      if (xxs === xs) {
        return cycle$1
      } else {
        const [tag_xs, val_xs] = xs
        if (tag_xs === 'Nil') {
          return cycle$1
        } else {
          return ['Cons', {1: val_xs[1], 2: _complete(val_xs[2])}]
        }
      }
    }
    return ['Cons', {1: xxs[1][1], 2: _complete(xxs[1][2])}]
  })
cycle(cycle(ᐟCons𛰙4ꓹ_ᐟCons𛰙2ꓹ_ᐟNil𛰚𛰚))
