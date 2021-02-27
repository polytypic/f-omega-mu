include PPrint

(* Constants *)

let arrow_right = utf8string "→"
let double_angle_lhs = utf8string "《"
let double_angle_rhs = utf8string "》"
let exists = utf8string "∃"
let for_all = utf8string "∀"
let greater_equal = utf8string "≥"
let lambda_lower = utf8string "λ"
let lambda_upper = utf8string "Λ"
let less_equal = utf8string "≤"
let logical_and = utf8string "∧"
let logical_not = utf8string "¬"
let logical_or = utf8string "∨"
let mu_lower = utf8string "μ"
let not_equal = utf8string "≠"

(* Grouping *)

let wrap wrapper doc = align doc |> wrapper |> group
let angles = wrap angles
let braces = wrap braces
let brackets = wrap brackets
let parens = wrap parens
let wrap_if wrapper cond doc = if cond then wrapper doc else doc
let angles_if = wrap_if angles
let braces_if = wrap_if braces
let brackets_if = wrap_if brackets
let parens_if = wrap_if parens

(* Rendering *)

let to_string ?(max_width = 0) doc =
  let buffer = Buffer.create 1000 in
  if max_width <= 0 then
    ToBuffer.compact buffer doc
  else
    ToBuffer.pretty 1.0 max_width buffer doc;
  Buffer.sub buffer 0 (Buffer.length buffer)

(* Optimizations *)

let break_0 = break 0
let break_1 = break 1
let comma_break_1 = concat [comma; break_1]
let space_equals = concat [space; equals]
let true' = utf8string "true"
let false' = utf8string "false"
