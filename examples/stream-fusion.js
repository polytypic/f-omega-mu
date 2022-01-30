'use strict'
const $1 = ['None']
const $2 = x => xs => ['Some', {1: x, 2: xs}]
const fold = xrr$1 => r$1 => $4 => {
  for (;;) {
    const xrr = xrr$1,
      r = r$1,
      $3 = $4
    const [$5, $6] = $3
    if ($5 === 'Some') {
      ;(r$1 = xrr($6[1])(r)), ($4 = $6[2])
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
const xs = iota($1)(5)
const S1 = ys$1 => $5 => {
  for (;;) {
    const ys = ys$1,
      $4 = $5
    const [$6, $7] = $4
    if ($6 === 'Some') {
      if (((($7[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = ['Some', {1: ((($7[1] + 1) | 0) * 2) | 0, 2: ys}]),
          ($5 = $7[2])
      } else {
        $5 = $7[2]
      }
    } else {
      return S2(ys)(xs)
    }
  }
}
const S2 = ys$1 => $5 => {
  for (;;) {
    const ys = ys$1,
      $4 = $5
    const [$6, $7] = $4
    if ($6 === 'Some') {
      if (((($7[1] + 1) | 0) % 2 | 0) === 1) {
        ;(ys$1 = ['Some', {1: ((($7[1] + 1) | 0) * 2) | 0, 2: ys}]),
          ($5 = $7[2])
      } else {
        $5 = $7[2]
      }
    } else {
      return fold($2)($1)(ys)
    }
  }
}
S1($1)(xs)
