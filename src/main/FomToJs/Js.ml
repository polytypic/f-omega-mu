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
let special_identifier = StringSet.of_list ["arguments"; "get"; "set"]

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

let is_illegal_id name = StringSet.mem name illegal_id
