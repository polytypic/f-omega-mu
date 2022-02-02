open FomBasis
include PPrint

(* Constants *)

let arrow_right = utf8string "→"
let double_angle_quote_lhs = utf8string "«"
let double_angle_quote_rhs = utf8string "»"
let exists = utf8string "∃"
let for_all = utf8string "∀"
let greater_equal = utf8string "≥"
let less_equal = utf8string "≤"
let logical_and = utf8string "∧"
let logical_not = utf8string "¬"
let logical_or = utf8string "∨"
let not_equal = utf8string "≠"
let pipe = utf8string "|"
let tick = utf8string "'"

(* Greek *)

let alpha_lower = utf8format "α"
let kappa_lower = utf8string "κ"
let lambda_lower = utf8string "λ"
let lambda_upper = utf8string "Λ"
let mu_lower = utf8string "μ"

(* Bracketing  *)

let empties = (empty, empty)
let spaces = (space, space)
let angles = (langle, rangle)
let braces = (lbrace, rbrace)
let brackets = (lbracket, rbracket)
let double_angle_quotes = (double_angle_quote_lhs, double_angle_quote_rhs)
let parens = (lparen, rparen)

(* *)

let softbreak_1 = group (break 1)

let text =
  String.split_on_char ' ' >>> List.map utf8string >>> separate softbreak_1

let textf fmt = Printf.ksprintf text fmt

(* Rendering *)

let to_string ?(max_width = 0) doc =
  let buffer = Buffer.create 1000 in
  if max_width <= 0 then ToBuffer.compact buffer doc
  else ToBuffer.pretty 1.0 max_width buffer doc;
  Buffer.sub buffer 0 (Buffer.length buffer)

(* Keywords *)

let bool' = utf8string "bool"
let case' = utf8string "case"
let else' = utf8string "else"
let false' = utf8string "false"
let if' = utf8string "if"
let in' = utf8string "in"
let int' = utf8string "int"
let keep' = utf8string "keep"
let let' = utf8string "let"
let string' = utf8string "string"
let target' = utf8string "target"
let then' = utf8string "then"
let true' = utf8string "true"
let type' = utf8string "type"

(* Optimizations *)

let break_0 = break 0
let break_0_0 = break_0 ^^ break_0
let break_1 = break 1
let break_1_0 = break_1 ^^ break_0
let pipe_space = pipe ^^ space
let break_1_pipe_space = break_1 ^^ pipe_space
let colon_break_1 = colon ^^ break_1
let colon_break_1_0 = colon_break_1 ^^ break_0
let comma_break_0 = comma ^^ break_0
let comma_break_1 = comma ^^ break_1
let comma_break_1_or_break_0 = ifflat comma_break_1 break_0
let comma_break_1_or_break_0_0 = ifflat comma_break_1 break_0_0
let let_space = let' ^^ space
let space_arrow_right = space ^^ arrow_right
let space_arrow_right_break_1 = space_arrow_right ^^ break_1
let space_equals = space ^^ equals
let space_equals_space = space_equals ^^ space
let space_in = space ^^ in'

(* *)

let egyptian (lhs, rhs) indent doc =
  group (lhs ^^ nest indent (break_0 ^^ doc) ^^ break_0 ^^ rhs)

let gnest n d = group @@ nest n d

(* *)

let sub_digit = [|"₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉"|]

let subscript n =
  if n < 0 then failwith "subscript";
  let rec loop s n =
    if n = 0 then if String.length s = 0 then sub_digit.(0) else s
    else
      let d = n mod 10 in
      let n = n / 10 in
      let s = sub_digit.(d) ^ s in
      loop s n
  in
  loop "" n |> utf8string
