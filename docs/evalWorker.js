'use strict'

onmessage = function (message) {
  onmessage = null
  eval(message.data)
}
