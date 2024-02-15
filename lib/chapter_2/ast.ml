[@@@ocaml.warning "-32-69"]

type t = { info : unit; body : expression } [@@deriving eq, show]

and expression =
  | Int of int
  (* | Let of { var : string; binding : expression; scope : expression } *)
  | Operation of operation (* | Var of string *)
[@@deriving eq, show]

and operation =
  | Add of expression * expression
  | Negate of expression
  | Read
  | Subtract of expression * expression
[@@deriving eq, show]

let to_string = show
let expression_to_string = show_expression
let operation_to_string = show_operation
