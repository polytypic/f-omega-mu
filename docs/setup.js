'use strict'

import {cmps, get, throttled} from './util.js'

import {examples} from './examples.js'

import {getWidth, getTokenEndingAt} from './cm-utils.js'

import {addMarker, clearMarkers, posAsNative} from './fom-cm-util.js'

import {onWorker} from './worker.js'

import {alternatives} from './fom-alternatives.js'

import './fom-cm-mode.js'

//

const url = `${location.origin}${location.pathname}examples/*`

//

const cmConfig = {
  cursorBlinkRate: 0,
  indentUnit: 2,
  lineNumbers: true,
  mode: 'fom',
  readOnly: true,
  tabSize: 2,
  theme: 'dracula',
}

const jsCM = CodeMirror(jsDiv, {...cmConfig, mode: 'javascript'})

const resultCM = CodeMirror(resultDiv, cmConfig)
const typCM = CodeMirror(typDiv, {
  ...cmConfig,
  lineNumbers: false,
  value: '...',
})
const fomCM = CodeMirror(fomDiv, {
  ...cmConfig,
  extraKeys: {
    Space() {
      let replacement = ' '
      if (replaceSymbolsInput.checked) {
        const newSelections = []
        for (const selection of fomCM.listSelections()) {
          const cursor = selection.head
          const {line, ch} = cursor
          const token = getTokenEndingAt(fomCM, cursor)
          if (!token) {
            replacement = ' '
            break
          }
          const alt = alternatives[token.string]
          if (!alt || (replacement !== ' ' && replacement !== alt)) {
            replacement = ' '
            break
          }
          replacement = alt

          newSelections.push({
            anchor: {line, ch: token.start},
            head: {line, ch: token.end},
          })
        }
        if (replacement !== ' ') fomCM.setSelections(newSelections)
      }
      fomCM.replaceSelection(replacement)
    },
    F2() {
      const cursor = fomCM.getCursor()
      const du = duAt(fomCM, cursor)
      if (du) {
        const selections = []
        let primary = 0
        const at = loc => {
          if (loc.file === url) {
            const begins = posAsNative(fomCM, loc.begins)
            const ends = posAsNative(fomCM, loc.ends)
            if (
              cursor.line === begins.line &&
              begins.ch <= cursor.ch &&
              cursor.ch <= ends.ch
            ) {
              primary = selections.length
            }
            selections.push({anchor: begins, head: ends})
          }
        }
        at(du.def)
        du.uses.forEach(at)
        fomCM.setSelections(selections, primary)
      }
    },
    Tab() {
      if (!maybeComplete()) fomCM.execCommand('defaultTab')
    },
  },
  readOnly: false,
})

const maybeComplete = (options = {}) => {
  if (fomCM.state.completionActive) return false

  const cursor = fomCM.getCursor()
  const token = getTokenEndingAt(fomCM, cursor)

  if (!token) return false

  fomCM.showHint({
    ...options,
    hint(cm, options) {
      const cursor = cm.getCursor()
      const {line} = cursor
      const {string: pat, start, end} = getTokenEndingAt(fomCM, cursor)

      const minLength = Math.max(1, pat.length - 2)
      const minLengthPr = s => minLength < s.length

      const replaceSymbols = replaceSymbolsInput.checked

      const uniqueIds = new Set([
        ...identifiers.filter(minLengthPr),
        ...fom.keywords.filter(minLengthPr),
        ...fom.pervasives.filter(minLengthPr),
        ...Object.keys(alternatives).filter(minLengthPr),
        ...currentDeps.flatMap(entry => entry.identifiers.filter(minLengthPr)),
      ])

      const list = Array.from(uniqueIds)
        .map(txt => [
          fom.distances(pat, txt, pat.toUpperCase(), txt.toUpperCase()),
          txt,
        ])
        .filter(([ds]) => !fom.distancesUnrelated(ds))
        .sort(([ds_l, txt_l], [ds_r, txt_r]) =>
          cmps(fom.distancesCompare(ds_l, ds_r), () =>
            txt_l.localeCompare(txt_r)
          )
        )
        .slice(0, 10)
        .map(([_, displayText]) => {
          const text = alternatives[displayText]
          return text && (replaceSymbols || displayText.startsWith('\\'))
            ? {displayText, text}
            : displayText
        })
      return {list, from: {line, ch: start}, to: {line, ch: end}}
    },
  })
  return true
}

const ignoredKeys = new Set([
  ' ',
  'ArrowDown',
  'ArrowLeft',
  'ArrowRight',
  'ArrowUp',
  'Backspace',
  'Delete',
  'Enter',
  'Escape',
  'Meta',
  'Shift',
  'Tab',
])

fomCM.on('keyup', (_, {key}) => {
  if (ignoredKeys.has(key)) return
  maybeComplete({completeSingle: false})
})

//

const currentDeps = []

let result = {typ: '...', defUses: [], diagnostics: [], dependencies: []}

//

const cmOf = targetFile => {
  if (url === targetFile) return fomCM
  for (const {file, cm} of currentDeps) if (file === targetFile) return cm
  return null
}

const fileOf = targetCM => {
  if (fomCM === targetCM) return url
  for (const {file, cm} of currentDeps) if (cm === targetCM) return file
  return null
}

//

const duMap = Object.create(null)

const insertDU = (loc, du) => {
  const file = duMap[loc.file] || (duMap[loc.file] = [])
  const begins = loc.begins
  const line = file[begins.line] || (file[begins.line] = [])
  line[begins.ch] = du
}

const unusedMarkers = []
const unusedAnnot = {
  css: 'text-decoration: underline tomato 2px',
  title: 'Unused binding',
}

const prepareDefUses = () => {
  clearMarkers(unusedMarkers)
  for (const file in duMap) {
    delete duMap[file]
  }
  result.defUses.forEach(du => du.uses.forEach(use => insertDU(use, du)))
  result.defUses.forEach(du => {
    insertDU(du.def, du)
    if (
      0 <= du.def.begins.line &&
      !du.annot.startsWith('_') &&
      du.uses.length === 0
    ) {
      const cm = cmOf(du.def.file)
      if (cm) addMarker(unusedMarkers, cm, du.def, unusedAnnot)
    }
  })
}

const duMarkers = []

const duAt = (cm, cursor, offset) => {
  if (undefined === offset) {
    return duAt(cm, cursor, 0) || duAt(cm, cursor, 1)
  } else {
    const token = cm.getTokenAt({line: cursor.line, ch: cursor.ch + offset})
    const file = fileOf(cm)
    return (
      file &&
      token &&
      token.start <= cursor.ch &&
      cursor.ch <= token.end &&
      get(
        duMap,
        file,
        cursor.line,
        fom.offset32(cm.getLine(cursor.line), token.start)
      )
    )
  }
}

const setTyp = (value, {noKeywords} = 0) => {
  if (typCM.getValue() !== value) {
    typDiv.className = noKeywords ? 'no-keywords show' : 'show'
    typCM.setValue(value)
    typDiv.classList.remove('show')
  }
}

const useAnnot = {css: 'background: saddlebrown'}
const defAnnot = {css: 'background: darkgreen'}

const updateDefUses = throttled(100, cm => {
  clearMarkers(duMarkers)
  const du = duAt(cm, cm.getCursor())
  if (du) {
    du.uses.forEach(use => {
      const cm = cmOf(use.file)
      if (cm) addMarker(duMarkers, cm, use, useAnnot)
    })
    const cm = cmOf(du.def.file)
    if (cm) addMarker(duMarkers, cm, du.def, defAnnot)
    setTyp(du.annot)
  } else {
    setTyp(result.typ, {noKeywords: result.diagnostics.length})
  }
})

fomCM.on('cursorActivity', updateDefUses)

//

const updateDeps = () => {
  if (
    result.dependencies.join(',') !== currentDeps.map(dep => dep.file).join(',')
  ) {
    currentDeps.length = 0

    while (depsDl.firstChild) depsDl.removeChild(depsDl.firstChild)

    if (result.dependencies.length === 0) {
      depsDiv.style.display = 'none'
    } else {
      depsDiv.style.display = 'block'

      for (const dep of result.dependencies) {
        const code = document.createElement('code')
        code.innerText = dep
        const dt = document.createElement('dt')
        dt.appendChild(code)
        depsDl.appendChild(dt)

        const dd = document.createElement('dd')
        depsDl.appendChild(dd)
        const depCM = CodeMirror(dd, cmConfig)

        depCM.on('cursorActivity', updateDefUses)

        const entry = {file: dep, cm: depCM, identifiers: []}

        const xhr = new XMLHttpRequest()
        xhr.onload = () => {
          const text = xhr.responseText.trim()
          entry.identifiers = fom.identifiers(text)
          depCM.setValue(text)
        }
        xhr.open('GET', dep)
        xhr.send()

        currentDeps.push(entry)
      }
    }
  }
}

//

const run = onWorker({
  init: () => {
    importScripts('FomSandbox.js')
    importScripts('prelude.js')
  },
  before: js => ({js, width: getWidth(fomCM)}),
  compute: (params, onResult) =>
    tryIn(
      () => {
        const result = withContext('running Fωμ', () =>
          timed('eval', () => eval(params.js))
        )
        return withContext('formatting output', () =>
          timed('format', () => fom.format(result, params.width))
        )
      },
      onResult,
      error => onResult(`${error}`)
    ),
  after: result => {
    if (typeof result !== 'string') result = ''
    resultCM.setValue(result)
  },
})

//

const diagnosticMarkers = []

const build = throttled(
  200,
  onWorker({
    init: () => {
      importScripts('FomSandbox.js')
      importScripts('https://unpkg.com/prettier@2.5.1/standalone.js')
      importScripts('https://unpkg.com/prettier@2.5.1/parser-babel.js')
      importScripts('https://unpkg.com/terser@5.12.0/dist/bundle.min.js')
    },
    before: () => {
      clearMarkers(diagnosticMarkers)
      return {
        url,
        whole: wholeSelect.checked,
        exp: fomCM.getValue(),
        terser: terserInput.checked,
        prettify: prettifyInput.checked,
        width: getWidth(fomCM),
      }
    },
    compute: ({url, whole, exp, terser, prettify, width}, onResult) => {
      let start = timingStart()
      fom.build(
        whole,
        url,
        exp,
        Math.min(width, 80),
        () => {
          timingEnd('elaborate', start)
          start = timingStart()
        },
        success => {
          timingEnd('def-use', start)
          onResult(success, true)
          start = timingStart()
        },
        failure => {
          timingEnd('defuses', start)
          onResult(failure)
        },
        async js => {
          timingEnd('compile', start)
          try {
            if (terser)
              js = (
                await Terser.minify(js, {
                  compress: {
                    expression: true,
                    keep_fargs: false,
                    pure_getters: true,
                  },
                  mangle: {
                    toplevel: true,
                  },
                  ecma: 2015,
                })
              ).code
            if (prettify)
              js = prettier
                .format(js, {
                  arrowParens: 'avoid',
                  bracketSpacing: false,
                  printWidth: width,
                  parser: 'babel',
                  plugins: [prettierPlugins.babel],
                  singleQuote: true,
                  trailingComma: 'none',
                })
                .trim()
            onResult(js)
          } catch (error) {
            console.error('Prettier failed with error:', error)
            onResult('')
          }
        }
      )
    },
    after: data => {
      if (typeof data === 'string') {
        jsCM.setValue(data)
        run(data)
      } else {
        result = data

        if (result.diagnostics.length) {
          resultCM.setValue('')
          jsCM.setValue('')

          result.diagnostics.forEach(diagnostic => {
            const cm = cmOf(diagnostic.file)
            if (cm) {
              const css = diagnosticMarkers.length
                ? 'text-shadow: 0px 0px 10px orange'
                : 'text-shadow: 0px 0px 10px red'
              addMarker(diagnosticMarkers, cm, diagnostic, {
                className: 'marker',
                css,
                title: diagnostic.message,
              })
            }
          })
        }

        prepareDefUses()
        updateDefUses(fomCM)

        if (!result.diagnostics.length) updateDeps()
      }
    },
  })
)

build()

fomCM.on('change', build)
wholeSelect.onclick = build
terserInput.onclick = build
prettifyInput.onclick = build

let lastWidth = getWidth(fomCM)
window.onresize = () => {
  const width = getWidth(fomCM)
  if (lastWidth !== width) {
    lastWidth = width
    build()
  }
}

//

fomCM.on(
  'change',
  throttled(
    100,
    onWorker({
      init: () =>
        importScripts(
          'https://cdnjs.cloudflare.com/ajax/libs/lz-string/1.4.4/lz-string.min.js'
        ),
      before: cm => cm.getValue(),
      compute: (exp, onResult) =>
        onResult(LZString.compressToEncodedURIComponent(exp)),
      after: data =>
        history.replaceState(null, '', location.pathname + '#' + data),
    })
  )
)

//

let identifiers = []

fomCM.on(
  'change',
  throttled(
    100,
    onWorker({
      init: () => {
        importScripts('FomSandbox.js')
      },
      before: cm => ({
        current: getTokenEndingAt(cm, cm.getCursor()),
        text: cm.getValue(),
      }),
      compute: ({current, text}, onResult) => {
        const ids = fom.identifiers(text)
        onResult(current ? ids.filter(id => id !== current.string) : ids)
      },
      after: ids => (identifiers = ids),
    })
  )
)

//

examples.forEach(example => {
  const option = document.createElement('option')
  option.value = example
  option.innerText = example.replace(/^.*[/](.*)[.].*$/, '$1').toUpperCase()
  exampleSelect.appendChild(option)
})

const exampleReset = () => {
  fomCM.off('change', exampleReset)
  exampleSelect.value = ''
}

exampleSelect.onchange = () => load(exampleSelect.value)

//

jsSelect.onchange = () => jsCM.refresh()

depsSelect.onchange = () => {
  for (const div of depsDl.querySelectorAll('dd > div'))
    div.CodeMirror.refresh()
}

//

editInput.onchange = () => fomCM.setOption('readOnly', !editInput.checked)

//

const load = path => {
  if (path) {
    const xhr = new XMLHttpRequest()
    xhr.onload = () => {
      if (200 <= xhr.status && xhr.status < 300) {
        fomCM.off('change', exampleReset)
        fomCM.setValue(xhr.responseText.trim())
        fomCM.on('change', exampleReset)
      } else {
        console.warn(`${xhr.statusText} when loading ${path}`)
      }
    }
    xhr.open('GET', path)
    xhr.send()
  }
}

const interpretHash = hash => {
  if (hash.startsWith('*')) {
    load(hash.slice(1))
  } else {
    fomCM.setValue(LZString.decompressFromEncodedURIComponent(hash) || '')
  }
}

interpretHash(location.hash.slice(1))
