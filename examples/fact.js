'use strict'
const fact = n => {
  if (n <= 1) {
    return 1
  } else {
    return (n * fact((n - 1) | 0)) | 0
  }
}
fact(5)
