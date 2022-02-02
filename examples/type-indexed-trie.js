'use strict'
const ᐟSome𛰙101𛰚 = ['Some', 101]
const λcsꓸcsꓸUnit_ᐟSome𛰙101𛰚 = cs => cs.Unit(ᐟSome𛰙101𛰚)
const ᐟSome𛰙42𛰚 = ['Some', 42]
const λcsꓸcsꓸUnit_ᐟSome𛰙42𛰚 = cs => cs.Unit(ᐟSome𛰙42𛰚)
const λcsꓸ𛰙csꓸAlt_𛰙λcsꓸcsꓸUnit_ = cs =>
  cs.Alt(λcsꓸcsꓸUnit_ᐟSome𛰙101𛰚)(λcsꓸcsꓸUnit_ᐟSome𛰙42𛰚)
const ᐟIn2 = ['In2']
const λvꓸλ_𛰙𛰚ꓸv = v => _𛰙𛰚 => v
const ᐟNone = ['None']
const lookup = t =>
  t({
    Unit: λvꓸλ_𛰙𛰚ꓸv,
    Alt: t1 => t2 => ᐟIn1_ǀ_ᐟIn2 => {
      const [tag_ᐟIn1_ǀ_ᐟIn2, val_ᐟIn1_ǀ_ᐟIn2] = ᐟIn1_ǀ_ᐟIn2
      if (tag_ᐟIn1_ǀ_ᐟIn2 === 'In2') {
        return lookup(t2)(val_ᐟIn1_ǀ_ᐟIn2)
      } else {
        return lookup(t1)(val_ᐟIn1_ǀ_ᐟIn2)
      }
    },
    Pair: t$1 => _𛰙k1ꓹ_k2𛰚 => {
      const [tag_𛰙lookup_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1, val_𛰙lookup_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1] =
        lookup(t$1)(_𛰙k1ꓹ_k2𛰚[1])
      if (tag_𛰙lookup_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1 === 'Some') {
        return lookup(val_𛰙lookup_t𛰚_𛰙_𛰙k1ꓹ_k2𛰚𛰚ꓸ1)(_𛰙k1ꓹ_k2𛰚[2])
      } else {
        return ᐟNone
      }
    },
  })
lookup(λcsꓸ𛰙csꓸAlt_𛰙λcsꓸcsꓸUnit_)(ᐟIn2)
