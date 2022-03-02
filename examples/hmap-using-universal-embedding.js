'use strict'
const ᐟNone = ['None']
const ᐟNil = ['Nil']
const ᐥ42ᐥ = '42'
const target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_ = r => x => {
  r[0] = x
}
const target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1 = r => r[0]
const target_α_𐙤_impure_𛰙ꓱrefꓽᕯ_ = x => [x]
const counter = target_α_𐙤_impure_𛰙ꓱrefꓽᕯ_(0)
const 𝛍getꓸλkꓸcase_𛰝Nilꘌλ_𛰙𛰚ꓸᐟNoneꓹ_ = k$1 => ᐟNil_ǀ_ᐟCons$1 => {
  for (;;) {
    const k = k$1,
      ᐟNil_ǀ_ᐟCons = ᐟNil_ǀ_ᐟCons$1
    const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
    if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
      return ᐟNone
    } else {
      const [tag_kꓸof_𛰙val_ᐟNil_ǀ_ᐟCons𛰚ꓸ1, val_kꓸof_𛰙val_ᐟNil_ǀ_ᐟCons𛰚ꓸ1] =
        k.of(val_ᐟNil_ǀ_ᐟCons[1])
      if (tag_kꓸof_𛰙val_ᐟNil_ǀ_ᐟCons𛰚ꓸ1 === 'Some') {
        return ['Some', val_kꓸof_𛰙val_ᐟNil_ǀ_ᐟCons𛰚ꓸ1]
      } else {
        ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2]
      }
    }
  }
}
const id = target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(counter)
target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_(counter)((id + 1) | 0)
const id$1 = target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(counter)
target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_(counter)((id$1 + 1) | 0)
const $1$ = 𝛍getꓸλkꓸcase_𛰝Nilꘌλ_𛰙𛰚ꓸᐟNoneꓹ_({
  to: x => ({id, value: x}),
  of: x => {
    if (x.id === id) {
      return ['Some', x.value]
    } else {
      return ᐟNone
    }
  },
})([
  'Cons',
  {1: {id: id$1, value: ᐥ42ᐥ}, 2: ['Cons', {1: {id, value: 101}, 2: ᐟNil}]},
])
const $2$ = 𝛍getꓸλkꓸcase_𛰝Nilꘌλ_𛰙𛰚ꓸᐟNoneꓹ_({
  to: x => ({id: id$1, value: x}),
  of: x => {
    if (x.id === id$1) {
      return ['Some', x.value]
    } else {
      return ᐟNone
    }
  },
})([
  'Cons',
  {1: {id: id$1, value: ᐥ42ᐥ}, 2: ['Cons', {1: {id, value: 101}, 2: ᐟNil}]},
])
;({1: $1$, 2: $2$})
