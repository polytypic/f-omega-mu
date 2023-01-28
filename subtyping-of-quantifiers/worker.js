export const onWorker = ({init, before, compute, after}) => {
  const code = `(() => {
  (${init})()
  const compute = ${compute}
  onmessage = ({data}) =>
    compute(data, (data, continues = false) => postMessage({data, continues}))
})()`

  let working = false
  let worker

  return (...args) => {
    if (working) {
      worker.terminate()
      worker = undefined
      working = false
    }
    if (worker === undefined) {
      worker = new Worker('worker-main.js')
      worker.postMessage(code)
      worker.onmessage = ({data: {data, continues}}) => {
        working = continues
        after(data)
      }
    }
    working = true
    worker.postMessage(before(...args))
  }
}
