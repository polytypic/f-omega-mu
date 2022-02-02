'use strict'
const λxꓸx = x => x
const ᐟZero𛰙λxꓸx𛰚 = ['Zero', λxꓸx]
const loop = n$1 => i$1 => {
  for (;;) {
    const n = n$1,
      i = i$1
    if (i <= 0) {
      return n
    } else {
      ;(n$1 = ['Succ', {1: λxꓸx, 2: n}]), (i$1 = (i - 1) | 0)
    }
  }
}
const _n = loop(ᐟZero𛰙λxꓸx𛰚)(10)
const loop$1 = n$1 => i$1 => {
  for (;;) {
    const n = n$1,
      i = i$1
    const [tag_n, val_n] = n
    if (tag_n === 'Zero') {
      return i
    } else {
      ;(n$1 = val_n[2]), (i$1 = (i + 1) | 0)
    }
  }
}
loop$1(_n)(0)
