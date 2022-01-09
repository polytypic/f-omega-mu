'use strict'
const loop = i$1 => j$1 => n$1 => {
  for (;;) {
    const i = i$1,
      j = j$1,
      n = n$1
    if (n <= 0) {
      return i
    } else {
      ;(i$1 = j), (j$1 = (i + j) | 0), (n$1 = (n - 1) | 0)
    }
  }
}
loop(0)(1)(10)
