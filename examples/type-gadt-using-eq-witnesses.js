'use strict'
const 𝛌xꓸx = x => x
const ᐟBool𛰙𝛌xꓸx𛰚 = ['Bool', 𝛌xꓸx]
const ᐟString𛰙𝛌xꓸx𛰚 = ['String', 𝛌xꓸx]
const ᐟInt𛰙𝛌xꓸx𛰚 = ['Int', 𝛌xꓸx]
const 𛰙𝛌xꓸxꓹ_ᐟString𛰙𝛌xꓸx𛰚ꓹ_ᐟInt𛰙𝛌xꓸx𛰚𛰚 = {
  1: 𝛌xꓸx,
  2: ᐟString𛰙𝛌xꓸx𛰚,
  3: ᐟInt𛰙𝛌xꓸx𛰚,
}
const ᐟAlt𛰙𝛌xꓸxꓹ_ᐟString𛰙𝛌xꓸx𛰚ꓹ_ = ['Alt', 𛰙𝛌xꓸxꓹ_ᐟString𛰙𝛌xꓸx𛰚ꓹ_ᐟInt𛰙𝛌xꓸx𛰚𛰚]
const 𛰙𝛌xꓸxꓹ_ᐟBool𛰙𝛌xꓸx𛰚ꓹ_ᐟAlt𛰙𝛌xꓸxꓹ_ = {
  1: 𝛌xꓸx,
  2: ᐟBool𛰙𝛌xꓸx𛰚,
  3: ᐟAlt𛰙𝛌xꓸxꓹ_ᐟString𛰙𝛌xꓸx𛰚ꓹ_,
}
const ᐟPair𛰙𝛌xꓸxꓹ_ᐟBool𛰙𝛌xꓸx𛰚ꓹ_ = ['Pair', 𛰙𝛌xꓸxꓹ_ᐟBool𛰙𝛌xꓸx𛰚ꓹ_ᐟAlt𛰙𝛌xꓸxꓹ_]
const ᐥfooᐥ = 'foo'
const ᐟIn1𛰙ᐥfooᐥ𛰚 = ['In1', ᐥfooᐥ]
const 𛰙falseꓹ_ᐟIn1𛰙ᐥfooᐥ𛰚𛰚 = {1: false, 2: ᐟIn1𛰙ᐥfooᐥ𛰚}
const target_ꓯαꓸα_𐙤_string_ᐥx_ꘌᐳ_ = x => '' + x
const target_string_𐙤_string_ᐥJSONꓸstringifyᐥ = JSON.stringify
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
        target_string_𐙤_string_ᐥJSONꓸstringifyᐥ(
          val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_(𝛌xꓸx)(x)
        )
    }
    case 'Pair': {
      const to_string_a = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[2])
      const to_string_b = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[3])
      return t => {
        const _𛰙aꓹ_b𛰚 = val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](𝛌xꓸx)(t)
        const s = to_string_b(_𛰙aꓹ_b𛰚[2])
        return ᐥ𛰙ᐥ + to_string_a(_𛰙aꓹ_b𛰚[1]) + ᐥꓹ_ᐥ + s + ᐥ𛰚ᐥ
      }
    }
    case 'Iso': {
      const to_string_b = _to_string(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[3])
      return t =>
        to_string_b(
          val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[2][1](
            val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](𝛌xꓸx)(t)
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
        ] = val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_[1](𝛌xꓸx)(t)
        if (tag_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ === 'In2') {
          return ᐥᐟIn2𛰙ᐥ + to_string_b(val_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_) + ᐥ𛰚ᐥ
        } else {
          return ᐥᐟIn1𛰙ᐥ + to_string_a(val_𛰙𛰙val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_) + ᐥ𛰚ᐥ
        }
      }
    }
    default: {
      return x =>
        target_ꓯαꓸα_𐙤_string_ᐥx_ꘌᐳ_(val_ᐟBool_ǀ_ᐟInt_ǀ_ᐟString_ǀ_(𝛌xꓸx)(x))
    }
  }
}
_to_string(ᐟPair𛰙𝛌xꓸxꓹ_ᐟBool𛰙𝛌xꓸx𛰚ꓹ_)(𛰙falseꓹ_ᐟIn1𛰙ᐥfooᐥ𛰚𛰚)
