'use strict'
const target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_ = a => i => {
  if (i < 0 || a.length <= i)
    throw new Error(`Array.sub: Index out of bounds: 0 <= ${i} < ${a.length}`)
  return a[i]
}
const target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_$1 = a => v => a.push(v)
const ᐟNil = ['Nil']
const ᐟB = ['B']
const 𛰙ᐟBꓹ_2𛰚 = {1: ᐟB, 2: 2}
const 𛰙𛰙ᐟBꓹ_2𛰚ꓹ_ᐟNil𛰚 = {1: 𛰙ᐟBꓹ_2𛰚, 2: ᐟNil}
const ᐟCons𛰙𛰙ᐟBꓹ_2𛰚ꓹ_ᐟNil𛰚 = ['Cons', 𛰙𛰙ᐟBꓹ_2𛰚ꓹ_ᐟNil𛰚]
const ᐟA = ['A']
const 𛰙ᐟAꓹ_1𛰚 = {1: ᐟA, 2: 1}
const 𛰙𛰙ᐟAꓹ_1𛰚ꓹ_ᐟCons𛰙𛰙ᐟBꓹ_2𛰚ꓹ_ = {1: 𛰙ᐟAꓹ_1𛰚, 2: ᐟCons𛰙𛰙ᐟBꓹ_2𛰚ꓹ_ᐟNil𛰚}
const ᐟCons𛰙𛰙ᐟAꓹ_1𛰚ꓹ_ᐟCons𛰙𛰙ᐟBꓹ_ = ['Cons', 𛰙𛰙ᐟAꓹ_1𛰚ꓹ_ᐟCons𛰙𛰙ᐟBꓹ_2𛰚ꓹ_]
const target_impure_𛰙ꓱarrayꓽᕯ_𐙤_ = []
const _fold = rxr$1 => r$1 => ᐟNil_ǀ_ᐟCons$1 => {
  for (;;) {
    const rxr = rxr$1,
      r = r$1,
      ᐟNil_ǀ_ᐟCons = ᐟNil_ǀ_ᐟCons$1
    const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
    if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
      return r
    } else {
      ;(r$1 = rxr(r)(val_ᐟNil_ǀ_ᐟCons[1])),
        (ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2])
    }
  }
}
const ys = target_impure_𛰙ꓱarrayꓽᕯ_𐙤_
_fold(_𛰙𛰚 => x => target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_$1(ys)(x))(void 0)(
  ᐟCons𛰙𛰙ᐟAꓹ_1𛰚ꓹ_ᐟCons𛰙𛰙ᐟBꓹ_
)
target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_(ys)(1)
