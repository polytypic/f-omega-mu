'use strict'
const λxꓸx = x => x
const λabꓸab_𛰙λxꓸx𛰚 = ab => ab(λxꓸx)
const λbcꓸλabꓸλfꓸ𛰙bc_𛰙λxꓸx𛰚𛰚_𛰙ab_ = bc => ab => f => bc(λxꓸx)(ab(f))
const 𛰝ofFoldꘌλxꓸxꓹ_ofLeibnizꘌλxꓸxꓹ_ = {
  ofFold: λxꓸx,
  ofLeibniz: λxꓸx,
  refl: λxꓸx,
  symm: λxꓸx,
  trans: λbcꓸλabꓸλfꓸ𛰙bc_𛰙λxꓸx𛰚𛰚_𛰙ab_,
  to: λabꓸab_𛰙λxꓸx𛰚,
  from: λabꓸab_𛰙λxꓸx𛰚,
}
const λbcꓸλabꓸλxꓸbc_𛰙ab_x𛰚 = bc => ab => x => bc(ab(x))
const λabꓸλhꓸ𛰙ab_𛰙λxꓸx𛰚𛰚_h = ab => h => ab(λxꓸx)(h)
const 𛰝ofFoldꘌλabꓸλhꓸ𛰙ab_𛰙λxꓸx𛰚𛰚_ = {
  ofFold: λabꓸλhꓸ𛰙ab_𛰙λxꓸx𛰚𛰚_h,
  ofLeibniz: λxꓸx,
  refl: λxꓸx,
  symm: λabꓸab_𛰙λxꓸx𛰚,
  trans: λbcꓸλabꓸλxꓸbc_𛰙ab_x𛰚,
  to: λxꓸx,
  from: λabꓸab_𛰙λxꓸx𛰚,
}
;({Leibniz: 𛰝ofFoldꘌλabꓸλhꓸ𛰙ab_𛰙λxꓸx𛰚𛰚_, Fold: 𛰝ofFoldꘌλxꓸxꓹ_ofLeibnizꘌλxꓸxꓹ_})
