type t =
  | Illegal of string
  | Eof
  (* Identifiers + literals *)
  | Identifier of string
  | Int of int
  (* Operators *)
  | Plus
  | Minus
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
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Read -> "Read"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | LParen -> "LParen"
  | RParen -> "RParen"
;;

let is_int = function Int _ -> true | _ -> false

let to_string_literal = function
  | Eof -> "Eof"
  | Illegal str -> Fmt.str "%s" str
  | Identifier str -> Fmt.str "%s" str
  | Int int -> Fmt.str "%d" int
  | Plus -> "+"
  | Minus -> "-"
  | Read -> "read"
  | LBracket -> "["
  | RBracket -> "]"
  | LParen -> "("
  | RParen -> ")"
;;

let pp fmt token =
  Fmt.pf fmt "Token: %s, Literal: '%s'" (to_string token)
    (to_string_literal token)
;;

let identifier_of_string = function "read" -> Read | str -> Identifier str
