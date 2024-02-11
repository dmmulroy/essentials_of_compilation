[@@@ocaml.warning "-32-69"]

type t = { info : unit; body : expression } [@@deriving eq, show]

and expression =
  | Int of int
  | Prim of { operation : operation; expressions : expression list }
[@@deriving eq, show]

and operation = Read | Subtract | Negate | Add [@@deriving eq, show]

let is_leaf = function
  | Int _ -> true
  | Prim { operation = Read; expressions = [] } -> true
  | Prim { operation = Negate; expressions = [ _ ] } -> false
  | Prim { operation = Subtract; expressions = [ _; _ ] } -> false
  | Prim { operation = Add; expressions = [ _; _ ] } -> false
  | _ -> false
;;

let rec is_valid_expression = function
  | Int _ -> true
  | Prim { operation = Read; expressions = [] } -> true
  | Prim { operation = Negate; expressions = [ expression ] } ->
      is_valid_expression expression
  | Prim { operation = Subtract; expressions = [ expression_1; expression_2 ] }
    ->
      is_valid_expression expression_1 && is_valid_expression expression_2
  | Prim { operation = Add; expressions = [ expression_1; expression_2 ] } ->
      is_valid_expression expression_1 && is_valid_expression expression_2
  | _ -> false
;;
