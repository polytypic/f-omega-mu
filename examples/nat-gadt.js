'use strict'
const λcsꓸcsꓸZero = cs => cs.Zero
const loop = n$1 => i$1 => {
  for (;;) {
    const n = n$1,
      i = i$1
    if (i <= 0) {
      return n
    } else {
      ;(n$1 = cs => cs.Succ(n)), (i$1 = (i - 1) | 0)
    }
  }
}
const _n = loop(λcsꓸcsꓸZero)(10)
const loop$1 = n => i => n({Zero: i, Succ: n$1 => loop$1(n$1)((i + 1) | 0)})
loop$1(_n)(0)
