'use strict'
const $1 = n => {
  if (n <= 0) {
    return 1
  } else {
    return (2 * $1((n - 1) | 0)) | 0
  }
}
$1(11)
