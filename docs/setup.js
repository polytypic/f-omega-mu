'use strict'

//

const throttled = function (ms, fn) {
  let timeout = null
  return function () {
    clearTimeout(timeout)
    timeout = setTimeout(fn, ms)
  }
}

const onWorker = function (init, before, onWorker, after) {
  const code =
    '(function () {\n' +
    '  (' +
    init.toString() +
    ')();\n' +
    '\n' +
    '  const onWorker = ' +
    onWorker.toString() +
    '  \n' +
    '  onmessage = function (message) {\n' +
    '    postMessage(onWorker(message.data))\n' +
    '  }\n' +
    '})()'

  let working = false
  let worker

  return function () {
    if (working) {
      worker.terminate()
      worker = undefined
      working = false
    }
    if (worker === undefined) {
      worker = new Worker('evalWorker.js')
      worker.postMessage(code)
      worker.onmessage = function (message) {
        working = false
        after(message.data)
      }
    }
    working = true
    worker.postMessage(before())
  }
}

function clearMarkers(markers) {
  markers.forEach(function (mark) {
    mark.clear()
  })
  markers.length = 0
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

CodeMirror.defineMode('fom', function () {
  return {
    token: function (stream) {
      const token = fom.token(stream.string.slice(stream.start))
      if (token.name === 'error') {
        stream.skipToEnd()
      } else {
        stream.start += token.begins
        stream.pos += token.ends
      }
      return token.name
    },
  }
})

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
  Tab: function () {
    const spaces = Array(fomCM.getOption('indentUnit') + 1).join(' ')
    fomCM.replaceSelection(spaces)
  },
})

//

let result = {defUses: [], diagnostics: []}

//

const duMap = []

const insertDU = function (loc, du) {
  const begins = loc.begins
  const line = duMap[begins.line] || (duMap[begins.line] = [])
  line[begins.ch] = du
}

const prepareDefUses = function () {
  duMap.length = 0
  result.defUses.forEach(function (du) {
    du.uses.forEach(function (use) {
      insertDU(use, du)
    })
    insertDU(du.def, du)
  })
}

const duMarkers = []

const duAt = function (line, ch, offset) {
  const token = fomCM.getTokenAt({line: line, ch: ch + offset})
  return (
    token &&
    token.start <= ch &&
    ch <= token.end &&
    duMap[line] &&
    duMap[line][token.start]
  )
}

const updateDefUses = throttled(100, function () {
  clearMarkers(duMarkers)
  const cursor = fomCM.getCursor()
  const line = cursor.line
  const ch = cursor.ch
  const du = duAt(line, ch, 0) || duAt(line, ch, 1)
  if (du) {
    duMarkers.push(
      fomCM.markText(du.def.begins, du.def.ends, {
        css: 'background: darkgreen',
      })
    )
    du.uses.forEach(function (use) {
      duMarkers.push(
        fomCM.markText(use.begins, use.ends, {
          css: 'background: blue',
        })
      )
    })
    typCM.setValue(du.annot)
  } else {
    typCM.setValue(result.typ)
  }
})

fomCM.on('cursorActivity', updateDefUses)

//

const run = onWorker(
  function () {
    importScripts('FomSandbox.js')
    importScripts('prelude.js')
  },
  function () {
    return jsCM.getValue()
  },
  function (js) {
    try {
      // TODO: Timeout
      return fom.format(eval(js))
    } catch (error) {
      return error.toString()
    }
  },
  function (result) {
    if (typeof result !== 'string') result = ''
    resultCM.setValue(result)
  }
)

//

const compile = onWorker(
  function () {
    importScripts('FomSandbox.js')
    importScripts('https://unpkg.com/prettier@2.2.1/standalone.js')
    importScripts('https://unpkg.com/prettier@2.2.1/parser-babel.js')
  },
  function () {
    return fomCM.getValue()
  },
  function (exp) {
    const js = fom.compile(exp)
    try {
      return prettier
        .format(js, {
          arrowParens: 'avoid',
          bracketSpacing: false,
          parser: 'babel',
          plugins: [prettierPlugins.babel],
          singleQuote: true,
          trailingComma: 'none',
        })
        .trim()
    } catch (error) {
      console.error('Prettier failed with error:', error)
    }
    return js
  },
  function (js) {
    jsCM.setValue(js)
    run(js)
  }
)

//

const diagnosticMarkers = []

const check = throttled(
  200,
  onWorker(
    function () {
      importScripts('FomSandbox.js')
    },
    function () {
      clearMarkers(diagnosticMarkers)
      return fomCM.getValue()
    },
    function (exp) {
      return fom.check(exp)
    },
    function (data) {
      result = data

      if (result.diagnostics.length) {
        resultCM.setValue('')
        jsCM.setValue('')

        result.diagnostics.forEach(function (diagnostic) {
          const css = fomCM.getAllMarks().length
            ? 'text-shadow: 0px 0px 10px orange; text-decoration: underline'
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

//

fomCM.on(
  'change',
  throttled(
    100,
    onWorker(
      function () {
        importScripts(
          'https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js'
        )
      },
      function () {
        return fomCM.getValue()
      },
      function (exp) {
        return LZString.compressToEncodedURIComponent(exp)
      },
      function (data) {
        history.replaceState(null, '', location.pathname + '#' + data)
      }
    )
  )
)

//

const replacements = {
  '!=': '≠',
  '&&': '∧',
  '->': '→',
  '<<': '《',
  '<=': '≤',
  '=>': '.',
  '>=': '≥',
  '>>': '》',
  exists: '∃',
  forall: '∀',
  fun: 'λ',
  gen: 'Λ',
  rec: 'μ',
  '||': '∨',
  '!': '¬',
}

fomCM.on('keyup', function (_, event) {
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
  }
})
