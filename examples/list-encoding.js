'use strict'
const 𐘁 = l => r => (l + r) | 0
const 𝛌cꓸcꓸnil = c => c.nil
const 𝛌cꓸ𛰙cꓸcons_1𛰚_𝛌cꓸcꓸnil = c => c.cons(1)(𝛌cꓸcꓸnil)
const 𝛌cꓸ𛰙cꓸcons_4𛰚_𛰙𝛌cꓸ𛰙cꓸcons_ = c => c.cons(4)(𝛌cꓸ𛰙cꓸcons_1𛰚_𝛌cꓸcꓸnil)
const 𝛌cꓸ𛰙cꓸcons_1𛰚_𛰙𝛌cꓸ𛰙cꓸcons_ = c => c.cons(1)(𝛌cꓸ𛰙cꓸcons_4𛰚_𛰙𝛌cꓸ𛰙cꓸcons_)
const 𝛌cꓸ𛰙cꓸcons_3𛰚_𛰙𝛌cꓸ𛰙cꓸcons_ = c => c.cons(3)(𝛌cꓸ𛰙cꓸcons_1𛰚_𛰙𝛌cꓸ𛰙cꓸcons_)
const _fold = fn => z => xs =>
  xs({nil: z, cons: x => xs$1 => _fold(fn)(fn(x)(z))(xs$1)})
_fold(𐘁)(0)(𝛌cꓸ𛰙cꓸcons_3𛰚_𛰙𝛌cꓸ𛰙cꓸcons_)
