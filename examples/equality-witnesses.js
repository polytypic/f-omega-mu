'use strict'
const 𝛌xꓸx = x => x
const 𝛌abꓸab_𝛌xꓸx = ab => ab(𝛌xꓸx)
const 𝛌bcꓸλabꓸλxꓸbc_𛰙ab_x𛰚 = bc => ab => x => bc(ab(x))
const 𛰝ofFoldꘌ𝛌abꓸab_𝛌xꓸxꓹ_ofLeibnizꘌ𝛌xꓸxꓹ_ = {
  ofFold: 𝛌abꓸab_𝛌xꓸx,
  ofLeibniz: 𝛌xꓸx,
  refl: 𝛌xꓸx,
  symm: 𝛌abꓸab_𝛌xꓸx,
  trans: 𝛌bcꓸλabꓸλxꓸbc_𛰙ab_x𛰚,
  to: 𝛌xꓸx,
  from: 𝛌abꓸab_𝛌xꓸx,
}
const 𝛌bcꓸλabꓸλfꓸ𛰙bc_𝛌xꓸx𛰚_𛰙ab_ = bc => ab => f => bc(𝛌xꓸx)(ab(f))
const 𛰝ofFoldꘌ𝛌xꓸxꓹ_ofLeibnizꘌ𝛌xꓸxꓹ_ = {
  ofFold: 𝛌xꓸx,
  ofLeibniz: 𝛌xꓸx,
  refl: 𝛌xꓸx,
  symm: 𝛌xꓸx,
  trans: 𝛌bcꓸλabꓸλfꓸ𛰙bc_𝛌xꓸx𛰚_𛰙ab_,
  to: 𝛌abꓸab_𝛌xꓸx,
  from: 𝛌abꓸab_𝛌xꓸx,
}
;({
  Leibniz: 𛰝ofFoldꘌ𝛌abꓸab_𝛌xꓸxꓹ_ofLeibnizꘌ𝛌xꓸxꓹ_,
  Fold: 𛰝ofFoldꘌ𝛌xꓸxꓹ_ofLeibnizꘌ𝛌xꓸxꓹ_,
})
