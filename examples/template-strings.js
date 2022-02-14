'use strict'
const ᐟNone = ['None']
const ᐥexperimentalᐥ = 'experimental'
const 𛰙ᐥexperimentalᐥꓹ_ᐟNone𛰚 = {1: ᐥexperimentalᐥ, 2: ᐟNone}
const ᐟSome𛰙ᐥexperimentalᐥꓹ_ᐟNone𛰚 = ['Some', 𛰙ᐥexperimentalᐥꓹ_ᐟNone𛰚]
const ᐥtemplate_stringsᐥ = 'template strings'
const 𛰙ᐥtemplate_stringsᐥꓹ_ᐟSome𛰙ᐥexperimentalᐥꓹ_ = {
  1: ᐥtemplate_stringsᐥ,
  2: ᐟSome𛰙ᐥexperimentalᐥꓹ_ᐟNone𛰚,
}
const ᐟSome𛰙ᐥtemplate_stringsᐥꓹ_ = [
  'Some',
  𛰙ᐥtemplate_stringsᐥꓹ_ᐟSome𛰙ᐥexperimentalᐥꓹ_,
]
const ᐥINSERT_INTO_language_VALUES_ = 'INSERT INTO language VALUES (?, ?)'
;({sql: ᐥINSERT_INTO_language_VALUES_, args: ᐟSome𛰙ᐥtemplate_stringsᐥꓹ_})
