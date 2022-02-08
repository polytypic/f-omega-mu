'use strict'
const λxꓸx = x => x
const ᐟBool𛰙λxꓸx𛰚 = ['Bool', λxꓸx]
const ᐟString𛰙λxꓸx𛰚 = ['String', λxꓸx]
const ᐟInt𛰙λxꓸx𛰚 = ['Int', λxꓸx]
const 𛰙λxꓸxꓹ_ᐟString𛰙λxꓸx𛰚ꓹ_ᐟInt𛰙λxꓸx𛰚𛰚 = {
  1: λxꓸx,
  2: ᐟString𛰙λxꓸx𛰚,
  3: ᐟInt𛰙λxꓸx𛰚,
}
const ᐟAlt𛰙λxꓸxꓹ_ᐟString𛰙λxꓸx𛰚ꓹ_ = ['Alt', 𛰙λxꓸxꓹ_ᐟString𛰙λxꓸx𛰚ꓹ_ᐟInt𛰙λxꓸx𛰚𛰚]
const 𛰙λxꓸxꓹ_ᐟBool𛰙λxꓸx𛰚ꓹ_ᐟAlt𛰙λxꓸxꓹ_ = {
  1: λxꓸx,
  2: ᐟBool𛰙λxꓸx𛰚,
  3: ᐟAlt𛰙λxꓸxꓹ_ᐟString𛰙λxꓸx𛰚ꓹ_,
}
const ᐟPair𛰙λxꓸxꓹ_ᐟBool𛰙λxꓸx𛰚ꓹ_ = ['Pair', 𛰙λxꓸxꓹ_ᐟBool𛰙λxꓸx𛰚ꓹ_ᐟAlt𛰙λxꓸxꓹ_]
const ᐥfooᐥ = 'foo'
const ᐟIn1𛰙ᐥfooᐥ𛰚 = ['In1', ᐥfooᐥ]
const 𛰙falseꓹ_ᐟIn1𛰙ᐥfooᐥ𛰚𛰚 = {1: false, 2: ᐟIn1𛰙ᐥfooᐥ𛰚}
const target𖩇ꓯαꓸα_𐙤_string𖩉_ᐥx_ = x => '' + x
const target𖩇string_𐙤_string𖩉_ᐥJSONꓸstringifyᐥ = JSON.stringify
const ᐥᐟIn1𛰙ᐥ = "'In1("
const ᐥ𛰚ᐥ = ')'
const ᐥᐟIn2𛰙ᐥ = "'In2("
const ᐥ𛰙ᐥ = '('
const ᐥꓹ_ᐥ = ', '
const _to_string = ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_ => {
  const [tag_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_, val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_] =
    ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_
  switch (tag_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_) {
    case 'String': {
      return x =>
        target𖩇string_𐙤_string𖩉_ᐥJSONꓸstringifyᐥ(
          val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_(λxꓸx)(x)
        )
    }
    case 'Pair': {
      const to_string_a = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[2])
      const to_string_b = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[3])
      return t => {
        const _𛰙aꓹ_b𛰚 = val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](λxꓸx)(t)
        const s = to_string_b(_𛰙aꓹ_b𛰚[2])
        return ᐥ𛰙ᐥ + to_string_a(_𛰙aꓹ_b𛰚[1]) + ᐥꓹ_ᐥ + s + ᐥ𛰚ᐥ
      }
    }
    case 'Iso': {
      const to_string_b = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[3])
      return t =>
        to_string_b(
          val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[2][1](
            val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](λxꓸx)(t)
          )
        )
    }
    case 'Alt': {
      const to_string_a = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[2])
      const to_string_b = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[3])
      return t => {
        const [
          tag_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_,
          val_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_,
        ] = val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](λxꓸx)(t)
        if (tag_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ === 'In2') {
          return ᐥᐟIn2𛰙ᐥ + to_string_b(val_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_) + ᐥ𛰚ᐥ
        } else {
          return ᐥᐟIn1𛰙ᐥ + to_string_a(val_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_) + ᐥ𛰚ᐥ
        }
      }
    }
    default: {
      return x =>
        target𖩇ꓯαꓸα_𐙤_string𖩉_ᐥx_(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_(λxꓸx)(x))
    }
  }
}
_to_string(ᐟPair𛰙λxꓸxꓹ_ᐟBool𛰙λxꓸx𛰚ꓹ_)(𛰙falseꓹ_ᐟIn1𛰙ᐥfooᐥ𛰚𛰚)
