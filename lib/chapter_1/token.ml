type t =
  | Illegal of string
  | Eof
  (* Identifiers + literals *)
  | Identifier of string
  | Int of int
  (* Operators *)
  | Add
  | Subtract
  | Read
  (* Delimiters *)
  | LBracket
  | RBracket
  | LParen
  | RParen
[@@deriving eq]

let to_string = function
  | Illegal str -> Fmt.str "Illegal(%s)" str
  | Eof -> "Eof"
  | Identifier str -> Fmt.str "Identifier(%s)" str
  | Int int -> Fmt.str "Int(%d)" int
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Read -> "Read"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | LParen -> "LParen"
  | RParen -> "RParen"
;;

let to_string_literal = function
  | Eof -> "Eof"
  | Illegal str -> Fmt.str "%s" str
  | Identifier str -> Fmt.str "%s" str
  | Int int -> Fmt.str "%d" int
  | Add -> "+"
  | Subtract -> "-"
  | Read -> "read"
  | LBracket -> "["
  | RBracket -> "]"
  | LParen -> "("
  | RParen -> ")"
;;

let pp fmt token =
  Fmt.pf fmt "Token: %s, Literal: %s" (to_string token)
    (to_string_literal token)
;;

let identifier_of_string = function "read" -> Read | str -> Identifier str
