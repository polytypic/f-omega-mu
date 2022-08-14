'use strict'
const 𝛌xꓸx = x => x
const _Id = {pure: 𝛌xꓸx, bind: xyE => xE => xyE(xE(_Id))(_Id)}
const _fib = n => {
  if (n <= 1) {
    return F => F.pure(n)
  } else {
    const _AppL = _fib((n - 2) | 0)
    return F =>
      F.bind(n2 => {
        const _AppL$1 = _fib((n - 1) | 0)
        return F$1 => F$1.bind(n1 => F$2 => F$2.pure((n2 + n1) | 0))(_AppL$1)
      })(_AppL)
  }
}
_fib(10)(_Id)
