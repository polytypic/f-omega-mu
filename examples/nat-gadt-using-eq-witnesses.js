'use strict'
const $1 = x => x
const $2 = ['Zero', $1]
const loop = n$1 => i$1 => {
  for (;;) {
    const n = n$1,
      i = i$1
    if (i <= 0) {
      return n
    } else {
      ;(n$1 = ['Succ', {1: $1, 2: n}]), (i$1 = (i - 1) | 0)
    }
  }
}
const $3 = loop($2)(10)
const loop$1 = n$1 => i$1 => {
  for (;;) {
    const n = n$1,
      i = i$1
    const [$4, $5] = n
    if ($4 === 'Zero') {
      return i
    } else {
      ;(n$1 = $5[2]), (i$1 = (i + 1) | 0)
    }
  }
}
loop$1($3)(0)
