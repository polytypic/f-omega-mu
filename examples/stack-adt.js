'use strict'
const ᐟNil = ['Nil']
const 𛰙3ꓹ_ᐟNil𛰚 = {1: 3, 2: ᐟNil}
const ᐟCons𛰙3ꓹ_ᐟNil𛰚 = ['Cons', 𛰙3ꓹ_ᐟNil𛰚]
const 𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚 = {1: 1, 2: ᐟCons𛰙3ꓹ_ᐟNil𛰚}
const ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚 = ['Cons', 𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚]
const 𛰙5ꓹ_ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚𛰚 = {1: 5, 2: ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚}
const ᐟCons𛰙5ꓹ_ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_ = ['Cons', 𛰙5ꓹ_ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_ᐟNil𛰚𛰚𛰚]
const _to_list = ᐟNil_ǀ_ᐟCons => {
  const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
  if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
    return ᐟNil
  } else {
    const $2$ = _to_list(val_ᐟNil_ǀ_ᐟCons[2])
    return ['Cons', {1: val_ᐟNil_ǀ_ᐟCons[1], 2: $2$}]
  }
}
_to_list(ᐟCons𛰙5ꓹ_ᐟCons𛰙1ꓹ_ᐟCons𛰙3ꓹ_)
