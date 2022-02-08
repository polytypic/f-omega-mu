'use strict'
const _fact = n => {
  if (n <= 1) {
    return 1
  } else {
    return (n * _fact((n - 1) | 0)) | 0
  }
}
_fact(5)
