'use strict'
const ᐟNone = ['None']
const ᐥ42ᐥ = '42'
const target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_ = r => x => {
  r[0] = x
}
const target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1 = r => r[0]
const target𖩇α_𐙤_impure_𛰙ꓱrefꓽᕯ_ = x => [x]
const target𖩇α_𐙤_impure_𛰙ꓱrefꓽᕯ_$1 = target𖩇α_𐙤_impure_𛰙ꓱrefꓽᕯ_(0)
const counter = target𖩇α_𐙤_impure_𛰙ꓱrefꓽᕯ_$1
const $get$ = k$1 => ᐟNone_ǀ_ᐟSome$1 => {
  for (;;) {
    const k = k$1,
      ᐟNone_ǀ_ᐟSome = ᐟNone_ǀ_ᐟSome$1
    const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
    if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
      const [tag_kꓸof_𛰙val_ᐟNone_ǀ_ᐟSome𛰚ꓸ1, val_kꓸof_𛰙val_ᐟNone_ǀ_ᐟSome𛰚ꓸ1] =
        k.of(val_ᐟNone_ǀ_ᐟSome[1])
      if (tag_kꓸof_𛰙val_ᐟNone_ǀ_ᐟSome𛰚ꓸ1 === 'Some') {
        return ['Some', val_kꓸof_𛰙val_ᐟNone_ǀ_ᐟSome𛰚ꓸ1]
      } else {
        ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2]
      }
    } else {
      return ᐟNone
    }
  }
}
const id = target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(counter)
target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_(counter)((id + 1) | 0)
const id$1 = target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(counter)
target𖩇𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_(counter)((id$1 + 1) | 0)
;({
  1: $get$({
    to: x => ({id, value: x}),
    of: x => {
      if (x.id === id) {
        return ['Some', x.value]
      } else {
        return ᐟNone
      }
    },
  })([
    'Some',
    {1: {id: id$1, value: ᐥ42ᐥ}, 2: ['Some', {1: {id, value: 101}, 2: ᐟNone}]},
  ]),
  2: $get$({
    to: x => ({id: id$1, value: x}),
    of: x => {
      if (x.id === id$1) {
        return ['Some', x.value]
      } else {
        return ᐟNone
      }
    },
  })([
    'Some',
    {1: {id: id$1, value: ᐥ42ᐥ}, 2: ['Some', {1: {id, value: 101}, 2: ᐟNone}]},
  ]),
})
