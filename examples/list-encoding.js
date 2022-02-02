'use strict'
const 𐘁 = l => r => (l + r) | 0
const λcꓸcꓸnil = c => c.nil
const λcꓸ𛰙cꓸcons_1𛰚_𛰙λcꓸcꓸnil𛰚 = c => c.cons(1)(λcꓸcꓸnil)
const λcꓸ𛰙cꓸcons_4𛰚_𛰙λcꓸ𛰙cꓸcons_ = c => c.cons(4)(λcꓸ𛰙cꓸcons_1𛰚_𛰙λcꓸcꓸnil𛰚)
const λcꓸ𛰙cꓸcons_1𛰚_𛰙λcꓸ𛰙cꓸcons_ = c => c.cons(1)(λcꓸ𛰙cꓸcons_4𛰚_𛰙λcꓸ𛰙cꓸcons_)
const λcꓸ𛰙cꓸcons_3𛰚_𛰙λcꓸ𛰙cꓸcons_ = c => c.cons(3)(λcꓸ𛰙cꓸcons_1𛰚_𛰙λcꓸ𛰙cꓸcons_)
const fold = fn => z => xs =>
  xs({nil: z, cons: x => xs$1 => fold(fn)(fn(x)(z))(xs$1)})
fold(𐘁)(0)(λcꓸ𛰙cꓸcons_3𛰚_𛰙λcꓸ𛰙cꓸcons_)
