'use strict'
const ᐟNone = ['None']
const 𛰙3ꓹ_ᐟNone𛰚 = {1: 3, 2: ᐟNone}
const ᐟSome𛰙3ꓹ_ᐟNone𛰚 = ['Some', 𛰙3ꓹ_ᐟNone𛰚]
const 𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚 = {1: 1, 2: ᐟSome𛰙3ꓹ_ᐟNone𛰚}
const ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚 = ['Some', 𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚]
const 𛰙5ꓹ_ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚𛰚 = {1: 5, 2: ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚}
const ᐟSome𛰙5ꓹ_ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_ = ['Some', 𛰙5ꓹ_ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_ᐟNone𛰚𛰚𛰚]
const _to_list = ᐟNone_ǀ_ᐟSome => {
  const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
  if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
    return [
      'Some',
      {1: val_ᐟNone_ǀ_ᐟSome[1], 2: _to_list(val_ᐟNone_ǀ_ᐟSome[2])},
    ]
  } else {
    return ᐟNone
  }
}
_to_list(ᐟSome𛰙5ꓹ_ᐟSome𛰙1ꓹ_ᐟSome𛰙3ꓹ_)
