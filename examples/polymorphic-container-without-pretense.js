'use strict'
const $1 = x => '' + x
const container = x => ({get: x, map: xy => container(xy(x))})
container(101).map($1)
