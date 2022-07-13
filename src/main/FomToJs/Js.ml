open StdlibPlus
open FomParser

let is_identity = ( = ) (JsonString.of_utf8 "x => x") (* TODO *)

module StringSet = Set.Make (String)

(* See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar *)

let reserved =
  StringSet.of_list
    [
      "break";
      "case";
      "catch";
      "class";
      "const";
      "continue";
      "debugger";
      "default";
      "delete";
      "do";
      "else";
      "export";
      "extends";
      "finally";
      "for";
      "function";
      "if";
      "import";
      "in";
      "instanceof";
      "new";
      "return";
      "super";
      "switch";
      "this";
      "throw";
      "try";
      "typeof";
      "var";
      "void";
      "while";
      "with";
      "yield";
    ]

let strict_mode_reserved =
  StringSet.of_list
    [
      "implements";
      "interface";
      "let";
      "package";
      "private";
      "protected";
      "public";
      "static";
      "yield";
    ]

let future_always_reserved = StringSet.of_list ["enum"]
let future_module_reserved = StringSet.of_list ["await"]
let literal = StringSet.of_list ["false"; "null"; "true"]
let special_identifier = StringSet.of_list ["arguments"; "eval"; "get"; "set"]

let illegal_id =
  [
    reserved;
    strict_mode_reserved;
    future_always_reserved;
    future_module_reserved;
    literal;
    special_identifier;
  ]
  |> List.fold_left StringSet.union StringSet.empty

let is_illegal_id name =
  StringSet.mem name illegal_id
  || match name.[0] with '0' .. '9' -> true | _ -> false

let max_safe_nat = "9007199254740991"

let is_safe_nat s =
  Lexer.is_nat s
  && (String.length s < String.length max_safe_nat
     || (String.length s = String.length max_safe_nat && s <= max_safe_nat))
