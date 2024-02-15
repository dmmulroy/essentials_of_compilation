open Ast
open Base
open Stdio

exception Invalid_ast_node of Ast.t
exception Invalid_expression of expression * string
exception Invalid_read_operation of string

let rec interpret_expression = function
  | Int n -> n
  | Operation (Add (left_expression, righ_expression)) ->
      let left = interpret_expression left_expression in
      let right = interpret_expression righ_expression in
      left + right
  | Operation (Subtract (left_expression, righ_expression)) ->
      let left = interpret_expression left_expression in
      let right = interpret_expression righ_expression in
      left - right
  | Operation (Negate expression) ->
      let result = interpret_expression expression in
      -result
  | Operation Read -> (
      Out_channel.print_string "Please enter an integer: ";
      match Stdlib.read_int_opt () with
      | Some n -> n
      | None -> raise (Invalid_read_operation "Read expected an integer"))
;;

(* | expression ->
    raise (Invalid_expression (expression, Ast.show_expression expression)) *)

let interpret { body; _ } = interpret_expression body

let partial_eval_negate = function
  | Int n -> Int (-n)
  | _ as expression -> Operation (Negate expression)
;;

let partial_eval_subtract = function
  | Int a, Int b -> Int (a - b)
  | left_expression, righ_expression ->
      Operation (Subtract (left_expression, righ_expression))
;;

let partial_eval_add = function
  | Int a, Int b -> Int (a + b)
  | left_expression, righ_expression ->
      Operation (Add (left_expression, righ_expression))
;;

let rec partial_eval_expression = function
  | Int n -> Int n
  | Operation (Add (left_expression, righ_expression)) ->
      partial_eval_add
        ( partial_eval_expression left_expression,
          partial_eval_expression righ_expression )
  | Operation (Subtract (left_expression, righ_expression)) ->
      partial_eval_subtract
        ( partial_eval_expression left_expression,
          partial_eval_expression righ_expression )
  | Operation (Negate expression) ->
      partial_eval_negate (partial_eval_expression expression)
  | Operation Read as read -> read
;;

let partial_eval { body; info } = { info; body = partial_eval_expression body }

(* Tests *)
let%test_module "l_int" =
  (module struct
    let ast1_1_with_read_value rv =
      Operation (Add (Int rv, Operation (Negate (Int 8))))
    ;;

    let%test_module "interp_exp" =
      (module struct
        let%test "(+ 60 9) is 69" =
          interpret_expression (Operation (Add (Int 60, Int 9))) = 69
        ;;

        let%test "ast1_1 is 42" =
          interpret_expression (ast1_1_with_read_value 50) = 42
        ;;
      end)
    ;;

    let%test_module "partial_eval" =
      (module struct
        let%test "partial_eval (Int 8) is (Int 8)" =
          let actual = partial_eval { info = (); body = Int 8 } in
          let expected = { info = (); body = Int 8 } in
          Ast.equal expected actual
        ;;

        let%test "(+ (read) (- (+ 5 3))) compiles to (+ (read) -8)" =
          let initial_program =
            {
              info = ();
              body =
                Operation
                  (Add
                     ( Operation Read,
                       Operation (Negate (Operation (Add (Int 5, Int 3)))) ));
            }
          in
          let expected =
            { info = (); body = Operation (Add (Operation Read, Int (-8))) }
          in
          let actual = partial_eval initial_program in
          Ast.equal expected actual
        ;;

        let%test "interp (program () (+ 10 (- (+ 5 3)))) = interp \
                  (partial_eval (program () (+ 10 (- (+ 5 3)))))" =
          let program =
            {
              info = ();
              body =
                Operation
                  (Add
                     ( Int 10,
                       Operation (Negate (Operation (Add (Int 5, Int 3)))) ));
            }
          in
          interpret program = interpret (partial_eval program)
        ;;

        let%test "interp (program () (+ 1 (+ 3 1))) = interp (partial_eval \
                  (program () (+ 1 (+ 3 1)))" =
          let program =
            {
              info = ();
              body = Operation (Add (Int 1, Operation (Add (Int 3, Int 1))));
            }
          in
          interpret program = interpret (partial_eval program)
        ;;

        let%test "interp (program () (- (+ 3 (- 5)))) = interp (partial_eval \
                  (program () (- (+ 3 (- 5)))))" =
          let program =
            {
              info = ();
              body =
                Operation
                  (Negate (Operation (Add (Int 3, Operation (Negate (Int 5))))));
            }
          in
          interpret program = interpret (partial_eval program)
        ;;
      end)
    ;;

    let%test "interpret (+ (10) (10)) is 20" =
      let program = { info = (); body = Operation (Add (Int 10, Int 10)) } in
      let expected = 20 in
      let actual = interpret program in
      expected = actual
    ;;

    let%test "interpret (+ (10) (9)) is 19" =
      let program = { info = (); body = Operation (Add (Int 10, Int 9)) } in
      let expected = 19 in
      let actual = interpret program in
      expected = actual
    ;;

    let%test "interpret (+ (69) (100)) is 169" =
      let program = { info = (); body = Operation (Add (Int 69, Int 100)) } in
      let expected = 169 in
      let actual = interpret program in
      expected = actual
    ;;
  end)
;;
