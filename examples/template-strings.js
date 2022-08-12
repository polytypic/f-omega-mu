'use strict'
const ᐥINSERT_INTO_language_VALUES_ = 'INSERT INTO language VALUES (?, ?)'
const ᐥtemplate_stringsᐥ = 'template strings'
const ᐥexperimentalᐥ = 'experimental'
const ᐟNil = ['Nil']
const 𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚 = {1: ᐥexperimentalᐥ, 2: ᐟNil}
const ᐟCons𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚 = ['Cons', 𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚]
const 𛰙ᐥtemplate_stringsᐥꓹ_ᐟCons𛰙ᐥexperimentalᐥꓹ_ = {
  1: ᐥtemplate_stringsᐥ,
  2: ᐟCons𛰙ᐥexperimentalᐥꓹ_ᐟNil𛰚,
}
const ᐟCons𛰙ᐥtemplate_stringsᐥꓹ_ = [
  'Cons',
  𛰙ᐥtemplate_stringsᐥꓹ_ᐟCons𛰙ᐥexperimentalᐥꓹ_,
]
;({sql: ᐥINSERT_INTO_language_VALUES_, args: ᐟCons𛰙ᐥtemplate_stringsᐥꓹ_})
