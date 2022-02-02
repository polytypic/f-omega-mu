'use strict'
const ᐟNone = ['None']
const λxꓸλxsꓸᐟSome𛰙xꓹ_xs𛰚 = x => xs => ['Some', {1: x, 2: xs}]
const fold = xrr$1 => r$1 => ᐟNone_ǀ_ᐟSome$1 => {
  for (;;) {
    const xrr = xrr$1,
      r = r$1,
      ᐟNone_ǀ_ᐟSome = ᐟNone_ǀ_ᐟSome$1
    const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
    if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
      ;(r$1 = xrr(val_ᐟNone_ǀ_ᐟSome[1])(r)),
        (ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2])
    } else {
      return r
    }
  }
}
const iota = xs$1 => n$1 => {
  for (;;) {
    const xs = xs$1,
      n = n$1
    if (0 < n) {
      ;(xs$1 = ['Some', {1: (n - 1) | 0, 2: xs}]), (n$1 = (n - 1) | 0)
    } else {
      return xs
    }
  }
}
const xs = iota(ᐟNone)(5)
const S1 = ys$1 => ᐟNone_ǀ_ᐟSome$1 => {
  for (;;) {
    const ys = ys$1,
      ᐟNone_ǀ_ᐟSome = ᐟNone_ǀ_ᐟSome$1
    const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
    if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
      if ((((val_ᐟNone_ǀ_ᐟSome[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = [
          'Some',
          {1: (((val_ᐟNone_ǀ_ᐟSome[1] + 1) | 0) * 2) | 0, 2: ys},
        ]),
          (ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2])
      } else {
        ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2]
      }
    } else {
      return S2(ys)(xs)
    }
  }
}
const S2 = ys$1 => ᐟNone_ǀ_ᐟSome$1 => {
  for (;;) {
    const ys = ys$1,
      ᐟNone_ǀ_ᐟSome = ᐟNone_ǀ_ᐟSome$1
    const [tag_ᐟNone_ǀ_ᐟSome, val_ᐟNone_ǀ_ᐟSome] = ᐟNone_ǀ_ᐟSome
    if (tag_ᐟNone_ǀ_ᐟSome === 'Some') {
      if ((((val_ᐟNone_ǀ_ᐟSome[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = [
          'Some',
          {1: (((val_ᐟNone_ǀ_ᐟSome[1] + 1) | 0) * 2) | 0, 2: ys},
        ]),
          (ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2])
      } else {
        ᐟNone_ǀ_ᐟSome$1 = val_ᐟNone_ǀ_ᐟSome[2]
      }
    } else {
      return fold(λxꓸλxsꓸᐟSome𛰙xꓹ_xs𛰚)(ᐟNone)(ys)
    }
  }
}
S1(ᐟNone)(xs)
