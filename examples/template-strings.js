'use strict'
const ᐟNil = ['Nil']
const ᐥexperimentalᐥ = 'experimental'
const 𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚 = {1: ᐥexperimentalᐥ, 2: ᐟNil}
const ᐟCons𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚 = ['Cons', 𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚]
const ᐥtemplate_stringsᐥ = 'template strings'
const 𛰙ᐥtemplate_stringsᐥꓹ_ᐟCons𛰙ᐥexperimentalᐥꓹ_ = {
  1: ᐥtemplate_stringsᐥ,
  2: ᐟCons𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚,
}
const ᐟCons𛰙ᐥtemplate_stringsᐥꓹ_ = [
  'Cons',
  𛰙ᐥtemplate_stringsᐥꓹ_ᐟCons𛰙ᐥexperimentalᐥꓹ_,
]
const ᐥINSERT_INTO_language_VALUES_ = 'INSERT INTO language VALUES (?, ?)'
;({sql: ᐥINSERT_INTO_language_VALUES_, args: ᐟCons𛰙ᐥtemplate_stringsᐥꓹ_})
