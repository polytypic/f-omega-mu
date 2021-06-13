'use strict'

FomSandbox(window)

//

const url = `${location.origin}${location.pathname}examples/*`

//

const throttled = (ms, fn) => {
  let timeout = null
  return () => {
    clearTimeout(timeout)
    timeout = setTimeout(fn, ms)
  }
}

const onWorker = (init, before, onWorker, after) => {
  const code = `(() => {
  (${init})()
  const onWorker = ${onWorker}
  onmessage = ({data}) => onWorker(data, postMessage)
})()`

  let working = false
  let worker

  return () => {
    if (working) {
      worker.terminate()
      worker = undefined
      working = false
    }
    if (worker === undefined) {
      worker = new Worker('evalWorker.js')
      worker.postMessage(code)
      worker.onmessage = message => {
        working = false
        after(message.data)
      }
    }
    working = true
    worker.postMessage(before())
  }
}

const clearMarkers = markers => {
  markers.forEach(mark => mark.clear())
  markers.length = 0
}

const getWidth = editor => {
  const charWidth = editor.defaultCharWidth()
  const scrollArea = editor.getScrollInfo()
  const scrollLeft = editor.doc.scrollLeft

  const leftColumn = Math.ceil(scrollLeft > 0 ? scrollLeft / charWidth : 0)
  const rightPosition = scrollLeft + (scrollArea.clientWidth - 30)
  const rightColumn = Math.floor(rightPosition / charWidth)

  return Math.max(0, rightColumn - leftColumn)
}

//

const theme = 'dracula'

const jsCM = CodeMirror(jsDiv, {
  cursorBlinkRate: 0,
  lineNumbers: true,
  mode: 'javascript',
  readOnly: true,
  theme: theme,
})

CodeMirror.defineMode('fom', () => ({
  token: stream => {
    const token = fom.token(stream.string.slice(stream.start))
    if (token.name === 'error') {
      stream.skipToEnd()
    } else {
      stream.start += token.begins
      stream.pos += token.ends
    }
    return token.name
  },
}))

const resultCM = CodeMirror(resultDiv, {
  cursorBlinkRate: 0,
  lineNumbers: true,
  mode: 'fom',
  readOnly: true,
  theme: theme,
})

const typCM = CodeMirror(typDiv, {
  cursorBlinkRate: 0,
  mode: 'fom',
  readOnly: true,
  theme: theme,
  value: '...',
})

const fomCM = CodeMirror(fomDiv, {
  autofocus: true,
  cursorBlinkRate: 0,
  indentUnit: 2,
  lineNumbers: true,
  mode: 'fom',
  tabSize: 2,
  theme: theme,
  value:
    LZString.decompressFromEncodedURIComponent(location.hash.slice(1)) || '',
})

fomCM.setOption('extraKeys', {
  Tab: () => {
    const spaces = Array(fomCM.getOption('indentUnit') + 1).join(' ')
    fomCM.replaceSelection(spaces)
  },
})

//

let result = {defUses: [], diagnostics: []}

//

const duMap = []

const insertDU = (loc, du) => {
  const begins = loc.begins
  const line = duMap[begins.line] || (duMap[begins.line] = [])
  line[begins.ch] = du
}

const prepareDefUses = () => {
  duMap.length = 0
  result.defUses.forEach(du => {
    du.uses.forEach(use => {
      if (use.file === url) {
        insertDU(use, du)
      }
    })
    if (du.def.file === url) {
      insertDU(du.def, du)
    }
  })
}

const duMarkers = []

const duAt = (cursor, offset) => {
  if (undefined === offset) {
    return duAt(cursor, 0) || duAt(cursor, 1)
  } else {
    const token = fomCM.getTokenAt({line: cursor.line, ch: cursor.ch + offset})
    return (
      token &&
      token.start <= cursor.ch &&
      cursor.ch <= token.end &&
      duMap[cursor.line] &&
      duMap[cursor.line][token.start]
    )
  }
}

const updateDefUses = throttled(100, () => {
  clearMarkers(duMarkers)
  const du = duAt(fomCM.getCursor())
  if (du) {
    if (du.def.file === url) {
      duMarkers.push(
        fomCM.markText(du.def.begins, du.def.ends, {
          css: 'background: darkgreen',
        })
      )
    }
    du.uses.forEach(use => {
      if (use.file === url) {
        duMarkers.push(
          fomCM.markText(use.begins, use.ends, {
            css: 'background: blue',
          })
        )
      }
    })
    typDiv.className = ''
    typCM.setValue(du.annot)
  } else {
    typDiv.className = result.diagnostics.length ? 'no-keywords' : ''
    typCM.setValue(result.typ || '...')
  }
})

fomCM.on('cursorActivity', updateDefUses)

//

const run = onWorker(
  () => {
    importScripts('FomSandbox.js')
    FomSandbox(self)
    importScripts('prelude.js')
  },
  () => ({js: jsCM.getValue(), width: getWidth(fomCM)}),
  (params, onResult) => {
    try {
      const result = timed('eval', () => eval(params.js))
      onResult(timed('format', () => fom.format(result, params.width)))
    } catch (error) {
      onResult(error.toString())
    }
  },
  result => {
    if (typeof result !== 'string') result = ''
    resultCM.setValue(result)
  }
)

//

const compile = onWorker(
  () => {
    importScripts('FomSandbox.js')
    FomSandbox(self)
    importScripts('https://unpkg.com/prettier@2.3.0/standalone.js')
    importScripts('https://unpkg.com/prettier@2.3.0/parser-babel.js')
  },
  () => ({url, exp: fomCM.getValue(), width: getWidth(fomCM)}),
  (params, onResult) => {
    const start = timingStart()
    fom.compile(params.url, params.exp, js => {
      timingEnd('compile', start)
      try {
        onResult(
          prettier
            .format(js, {
              arrowParens: 'avoid',
              bracketSpacing: false,
              printWidth: params.width,
              parser: 'babel',
              plugins: [prettierPlugins.babel],
              singleQuote: true,
              trailingComma: 'none',
            })
            .trim()
        )
      } catch (error) {
        console.error('Prettier failed with error:', error)
      }
    })
  },
  js => {
    jsCM.setValue(js)
    run(js)
  }
)

//

const diagnosticMarkers = []

const check = throttled(
  200,
  onWorker(
    () => {
      importScripts('FomSandbox.js')
      FomSandbox(self)
    },
    () => {
      clearMarkers(diagnosticMarkers)
      const width = Math.min(80, (getWidth(fomCM) * 0.85) | 0)
      return {url, exp: fomCM.getValue(), width: width}
    },
    (params, onResult) => {
      const start = timingStart()
      fom.check(params.url, params.exp, params.width, result => {
        timingEnd('check', start)
        onResult(result)
      })
    },
    data => {
      result = data

      if (result.diagnostics.length) {
        resultCM.setValue('')
        jsCM.setValue('')

        result.diagnostics.forEach(diagnostic => {
          const css = fomCM.getAllMarks().length
            ? 'text-shadow: 0px 0px 10px orange'
            : 'text-shadow: 0px 0px 10px red'
          diagnosticMarkers.push(
            fomCM.markText(diagnostic.begins, diagnostic.ends, {
              className: 'marker',
              css: css,
              title: diagnostic.message,
            })
          )
        })
      } else {
        compile()
      }

      prepareDefUses()
      updateDefUses()
    }
  )
)

check()

fomCM.on('change', check)

let lastWidth = getWidth(fomCM)
window.onresize = () => {
  const width = getWidth(fomCM)
  if (lastWidth !== width) {
    lastWidth = width
    check()
  }
}

//

fomCM.on(
  'change',
  throttled(
    100,
    onWorker(
      () =>
        importScripts(
          'https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js'
        ),
      () => fomCM.getValue(),
      (exp, onResult) => onResult(LZString.compressToEncodedURIComponent(exp)),
      data => history.replaceState(null, '', location.pathname + '#' + data)
    )
  )
)

//

const replacements = {
  '!': '¬',
  '!=': '≠ ',
  '&&': '∧ ',
  '->': '→ ',
  '<<': '《',
  '<=': '≤ ',
  '<|': '◁ ',
  '=>': '.',
  '>=': '≥ ',
  '>>': '》',
  '|>': '▷ ',
  '||': '∨ ',
  exists: '∃',
  forall: '∀',
  fun: 'λ',
  gen: 'Λ',
  rec: 'μ',
}

fomCM.on('keyup', (_, event) => {
  if (event.key === ' ' && replaceSymbolsInput.checked) {
    const cursor = fomCM.getCursor()
    const line = cursor.line
    const ch = cursor.ch - 1
    const token = fomCM.getTokenAt({line: line, ch: ch})
    if (!token || token.end !== ch) return
    const replacement = replacements[token.string]
    if (replacement) {
      fomCM.setSelection(
        {line: line, ch: token.start},
        {line: line, ch: ch + 1}
      )
      fomCM.replaceSelection(replacement)
    }
  } else if (event.key === 'F2') {
    const cursor = fomCM.getCursor()
    const du = duAt(cursor)
    if (du) {
      const selections = []
      let primary = 0
      const at = loc => {
        if (loc.file === url) {
          if (
            cursor.line === loc.begins.line &&
            loc.begins.ch <= cursor.ch &&
            cursor.ch <= loc.ends.ch
          ) {
            primary = selections.length
          }
          selections.push({anchor: loc.begins, head: loc.ends})
        }
      }
      at(du.def)
      du.uses.forEach(at)
      fomCM.setSelections(selections, primary)
    }
  }
})

//

examples.forEach(example => {
  const option = document.createElement('option')
  option.value = example
  option.innerText = example.replace(/^.*[/](.*)[.].*$/, '$1').toUpperCase()
  exampleSelect.appendChild(option)
})

exampleSelect.onchange = () => {
  const value = exampleSelect.value
  if (value) {
    const xhr = new XMLHttpRequest()
    xhr.onload = () => fomCM.setValue(xhr.responseText.trim())
    xhr.open('GET', value)
    xhr.send()
  }
}
