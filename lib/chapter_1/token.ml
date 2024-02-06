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
  | Illegal str -> Printf.sprintf "Illegal(%s)" str
  | Eof -> "Eof"
  | Identifier str -> Printf.sprintf "Identifier(%s)" str
  | Int int -> Printf.sprintf "Int(%s)" int
  | Add -> "+"
  | Negate -> "-"
  | LBracket -> "["
  | RBracket -> "]"
  | LParen -> "("
  | RParen -> ")"
  | Read -> "read"
  | Program -> "program"
;;

let identifier_of_string = function
  | "read" -> Read
  | "program" -> Program
  (* TODO: Revisit whether this should Identifier(string) or Illegal(string)*)
  | str -> Identifier str
;;
