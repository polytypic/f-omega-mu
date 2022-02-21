'use strict'
const ᐟSome𛰙101𛰚 = ['Some', 101]
const 𝛌csꓸcsꓸUnit_ᐟSome𛰙101𛰚 = cs => cs.Unit(ᐟSome𛰙101𛰚)
const ᐟSome𛰙42𛰚 = ['Some', 42]
const 𝛌csꓸcsꓸUnit_ᐟSome𛰙42𛰚 = cs => cs.Unit(ᐟSome𛰙42𛰚)
const 𝛌csꓸ𛰙csꓸAlt_𛰙𝛌csꓸcsꓸUnit_ = cs =>
  cs.Alt(𝛌csꓸcsꓸUnit_ᐟSome𛰙101𛰚)(𝛌csꓸcsꓸUnit_ᐟSome𛰙42𛰚)
const ᐟIn2 = ['In2']
const ᐟNone = ['None']
const 𝛌vꓸλ_𛰙𛰚ꓸv = v => _𛰙𛰚 => v
const _lookup = t =>
  t({
    Unit: 𝛌vꓸλ_𛰙𛰚ꓸv,
    Alt: t1 => t2 => ᐟIn1_ǀ_ᐟIn2 => {
      const [tag_ᐟIn1_ǀ_ᐟIn2, val_ᐟIn1_ǀ_ᐟIn2] = ᐟIn1_ǀ_ᐟIn2
      if (tag_ᐟIn1_ǀ_ᐟIn2 === 'In2') {
        return _lookup(t2)(val_ᐟIn1_ǀ_ᐟIn2)
      } else {
        return _lookup(t1)(val_ᐟIn1_ǀ_ᐟIn2)
      }
    },
    Pair: t$1 => _𛰙k1ꓹ_k2𛰚 => {
      const [tag_𛰙𛰙_lookup𛰚_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1, val_𛰙𛰙_lookup𛰚_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1] =
        _lookup(t$1)(_𛰙k1ꓹ_k2𛰚[1])
      if (tag_𛰙𛰙_lookup𛰚_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1 === 'Some') {
        return _lookup(val_𛰙𛰙_lookup𛰚_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1)(_𛰙k1ꓹ_k2𛰚[2])
      } else {
        return ᐟNone
      }
    },
  })
_lookup(𝛌csꓸ𛰙csꓸAlt_𛰙𝛌csꓸcsꓸUnit_)(ᐟIn2)
