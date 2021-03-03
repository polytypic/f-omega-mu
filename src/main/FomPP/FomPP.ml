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

(* Bracketing  *)

let empties = (empty, empty)
let spaces = (space, space)
let angles = (langle, rangle)
let braces = (lbrace, rbrace)
let brackets = (lbracket, rbracket)
let double_angles = (double_angle_lhs, double_angle_rhs)
let parens = (lparen, rparen)

(* Rendering *)

let to_string ?(max_width = 0) doc =
  let buffer = Buffer.create 1000 in
  if max_width <= 0 then
    ToBuffer.compact buffer doc
  else
    ToBuffer.pretty 1.0 max_width buffer doc;
  Buffer.sub buffer 0 (Buffer.length buffer)

(* Keywords *)

let bool' = utf8string "bool"
let case' = utf8string "case"
let else' = utf8string "else"
let false' = utf8string "false"
let if' = utf8string "if"
let in' = utf8string "in"
let int' = utf8string "int"
let let' = utf8string "let"
let string' = utf8string "string"
let target' = utf8string "target"
let then' = utf8string "then"
let true' = utf8string "true"
let type' = utf8string "type"

(* Optimizations *)

let break_0 = break 0
let break_1 = break 1
let comma_break_1 = [comma; break_1] |> concat
let let_space = [let'; space] |> concat
let space_arrow_right = [space; arrow_right] |> concat
let space_arrow_right_break_1 = [space_arrow_right; break_1] |> concat
let space_equals = [space; equals] |> concat
let space_equals_space = [space; equals; space] |> concat
let space_in = [space; in'] |> concat

(* *)

let egyptian (lhs, rhs) indent doc =
  [lhs; [break_0; doc] |> concat |> nest indent; break_0; rhs]
  |> concat |> group
