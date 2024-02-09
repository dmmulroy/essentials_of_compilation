[@@@ocaml.warning "-32-69"]

type t = { info : unit; body : expression }

and expression =
  | Int of int
  | Prim of { operation : operation; expressions : expression list }

and operation = Read | Subtract | Add

let operation_to_string = function
  | Read -> "read ()"
  | Subtract -> "-"
  | Add -> "+"
;;

let rec expression_to_string = function
  | Int i -> string_of_int i
  | Prim { operation; expressions } ->
      Fmt.str "(Prim %s (%s))"
        (operation_to_string operation)
        (String.concat " " (List.map expression_to_string expressions))
;;

let to_string { body; _ } =
  Fmt.str "(Program '() (%s))" (expression_to_string body)
;;