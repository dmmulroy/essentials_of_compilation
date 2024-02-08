type t =
  | Illegal of string
  | Eof
  (* Identifiers + literals *)
  | Identifier of string
  | Int of string
  (* Operators *)
  | Add
  | Negate
  (* Delimiters *)
  | LBracket
  | RBracket
  | LParen
  | RParen
  (* Keywords *)
  | Read
  | Program
[@@deriving eq]

let to_string = function
  | Illegal str -> Fmt.str "Illegal(%s)" str
  | Eof -> "Eof"
  | Identifier str -> Fmt.str "Identifier(%s)" str
  | Int int -> Fmt.str "Int(%s)" int
  | Add -> "Add"
  | Negate -> "Negate"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Read -> "Read"
  | Program -> "Program"
;;

let to_string_literal = function
  | Eof -> "Eof"
  | Illegal str -> Fmt.str "%s" str
  | Identifier str -> Fmt.str "%s" str
  | Int int -> Fmt.str "%s" int
  | Add -> "+"
  | Negate -> "-"
  | LBracket -> "["
  | RBracket -> "]"
  | LParen -> "("
  | RParen -> ")"
  | Read -> "read"
  | Program -> "program"
;;

let pp fmt token =
  Fmt.pf fmt "Token: %s, Literal: %s" (to_string token)
    (to_string_literal token)
;;

let identifier_of_string = function
  | "read" -> Read
  | "program" -> Program
  | str -> Identifier str
;;
