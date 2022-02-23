'use strict'
const ᐟNil = ['Nil']
const 𝛌xsꓸλxꓸᐟCons𛰙xꓹ_xs𛰚 = xs => x => ['Cons', {1: x, 2: xs}]
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
const _iota = xs$1 => n$1 => {
  for (;;) {
    const xs = xs$1,
      n = n$1
    if (0 < n) {
      ;(xs$1 = ['Cons', {1: (n - 1) | 0, 2: xs}]), (n$1 = (n - 1) | 0)
    } else {
      return xs
    }
  }
}
const xs = _iota(ᐟNil)(5)
const S1 = ys$1 => ᐟNil_ǀ_ᐟCons$1 => {
  for (;;) {
    const ys = ys$1,
      ᐟNil_ǀ_ᐟCons = ᐟNil_ǀ_ᐟCons$1
    const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
    if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
      return S2(ys)(xs)
    } else {
      if ((((val_ᐟNil_ǀ_ᐟCons[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = [
          'Cons',
          {1: (((val_ᐟNil_ǀ_ᐟCons[1] + 1) | 0) * 2) | 0, 2: ys},
        ]),
          (ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2])
      } else {
        ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2]
      }
    }
  }
}
const S2 = ys$1 => ᐟNil_ǀ_ᐟCons$1 => {
  for (;;) {
    const ys = ys$1,
      ᐟNil_ǀ_ᐟCons = ᐟNil_ǀ_ᐟCons$1
    const [tag_ᐟNil_ǀ_ᐟCons, val_ᐟNil_ǀ_ᐟCons] = ᐟNil_ǀ_ᐟCons
    if (tag_ᐟNil_ǀ_ᐟCons === 'Nil') {
      return _fold(𝛌xsꓸλxꓸᐟCons𛰙xꓹ_xs𛰚)(ᐟNil)(ys)
    } else {
      if ((((val_ᐟNil_ǀ_ᐟCons[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = [
          'Cons',
          {1: (((val_ᐟNil_ǀ_ᐟCons[1] + 1) | 0) * 2) | 0, 2: ys},
        ]),
          (ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2])
      } else {
        ᐟNil_ǀ_ᐟCons$1 = val_ᐟNil_ǀ_ᐟCons[2]
      }
    }
  }
}
S1(ᐟNil)(xs)
