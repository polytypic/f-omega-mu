'use strict'
const $1 = x => {
  if (x <= 0) {
    return 1
  } else {
    return (2 * $1((x - 1) | 0)) | 0
  }
}
$1(11)
