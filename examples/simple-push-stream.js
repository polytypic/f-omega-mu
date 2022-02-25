'use strict'
const target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_ = r => x => {
  r[0] = x
}
const target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1 = r => r[0]
const target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_ = a => i => {
  if (i < 0 || a.length <= i)
    throw new Error(`Array.sub: Index out of bounds: 0 <= ${i} < ${a.length}`)
  return a[i]
}
const target_α_𐙤_impure_𛰙ꓱrefꓽᕯ_ = x => [x]
const target_ꓯαꓽκꓸ𛰙ꓱarrayꓽκ_𐙤_ᕯꓸarray_ = a => a.length
const target_ꓯαꓸ𛰙int_𐙤_α𛰚_𐙤_int_ = fn => n => {
  const a = new Array(n)
  for (let i = 0; i < n; ++i) a[i] = fn(i)
  return a
}
const 𝛌iꓸi = i => i
const _AppL = target_ꓯαꓸ𛰙int_𐙤_α𛰚_𐙤_int_(𝛌iꓸi)(10)
const n = target_ꓯαꓽκꓸ𛰙ꓱarrayꓽκ_𐙤_ᕯꓸarray_(_AppL)
const sum = target_α_𐙤_impure_𛰙ꓱrefꓽᕯ_(0)
const _loop = i$1 => {
  for (;;) {
    const i = i$1
    if (i < n) {
      const x = target_ꓯαꓸ𛰙ꓱarrayꓽᕯ_𐙤_ᕯꓸarray_(_AppL)(i)
      if ((x % 2 | 0) !== 0) {
        const x$1 = (((x * x) | 0) + target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(sum)) | 0
        target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_(sum)(x$1)
      } else {
      }
      i$1 = (i + 1) | 0
    } else {
      return target_𛰙ꓱrefꓽᕯ_𐙤_ᕯꓸref_α𛰚_$1(sum)
    }
  }
}
_loop(0)
