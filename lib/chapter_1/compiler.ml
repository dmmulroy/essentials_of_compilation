open Ast

exception Invalid_ast_node of Ast.t
exception Invalid_expression of expression
exception Invalid_read_operation of string

let is_leaf = function
  | Int _ -> true
  | Prim { operation = Read; expressions = [] } -> true
  | Prim { operation = Subtract; expressions = [ _ ] } -> false
  | Prim { operation = Add; expressions = [ _; _ ] } -> false
  | _ -> false
;;

let rec is_valid_expression = function
  | Int _ -> true
  | Prim { operation = Read; expressions = [] } -> true
  | Prim { operation = Subtract; expressions = [ expression ] } ->
      is_valid_expression expression
  | Prim { operation = Add; expressions = [ expression_1; expression_2 ] } ->
      is_valid_expression expression_1 && is_valid_expression expression_2
  | _ -> false
;;

let rec interpret_expression = function
  | Int n -> n
  | Prim { operation = Read; expressions = [] } -> (
      match read_int_opt () with
      | Some n -> n
      | None -> raise (Invalid_read_operation "Read expected an integer"))
  | Prim { operation = Subtract; expressions = [ expression ] } ->
      -interpret_expression expression
  | Prim { operation = Add; expressions = [ expression_1; expression_2 ] } ->
      interpret_expression expression_1 + interpret_expression expression_2
  | expression -> raise (Invalid_expression expression)
;;

let interpret { body; _ } = interpret_expression body

let partial_eval_negate = function
  | Int n -> Int (-n)
  | _ as expression ->
      Prim { operation = Subtract; expressions = [ expression ] }
;;

let partial_eval_add = function
  | Int n, Int m -> Int (n + m)
  | expression_1, expression_2 ->
      Prim { operation = Add; expressions = [ expression_1; expression_2 ] }
;;

let rec partial_eval_expression = function
  | Int n -> Int n
  | Prim { operation = Read; expressions = [] } as read -> read
  | Prim { operation = Subtract; expressions = [ expression ] } ->
      partial_eval_negate (partial_eval_expression expression)
  | Prim { operation = Add; expressions = [ expression_1; expression_2 ] } ->
      partial_eval_add
        ( partial_eval_expression expression_1,
          partial_eval_expression expression_2 )
  | expression -> raise (Invalid_expression expression)
;;

let partial_eval { body; _ } = partial_eval_expression body

(* Tests *)
(* let%test_module "rint" =
     (module struct
       let ast1_1 = Prim (Add, [ Prim (Read, []); Prim (Subtract, [ Int 8 ]) ])

       let ast1_1_with_read_value rv =
         Prim (Add, [ Int rv; Prim (Subtract, [ Int 8 ]) ])
       ;;

       let%test_module "is_leaf" =
         (module struct
           let%test "Read [] is a leaf" = is_leaf (Prim (Read, [])) = true
           let%test "Int 8 is a leaf" = is_leaf (Int 8) = true

           let%test "Subtract (Int 8) is not a leaf" =
             is_leaf (Prim (Subtract, [ Int 8 ])) = false
           ;;
         end)
       ;;

       let%test_module "is_rint" =
         (module struct
           let%test "ast1_1 is a valid rint program" =
             is_rint (Program ((), ast1_1)) = true
           ;;

           let invalid_program =
             Program ((), Prim (Subtract, [ Prim (Read, []); Prim (Add, [ Int 8 ]) ]))
           ;;

           let%test "Invalid rint program" = is_rint invalid_program = false
         end)
       ;;

       let%test_module "interp_exp" =
         (module struct
           let%test "(+ 60 9) is 69" =
             interp_exp (Prim (Add, [ Int 60; Int 9 ])) = 69
           ;;

           let%test "ast1_1 is 42" = interp_exp (ast1_1_with_read_value 50) = 42
         end)
       ;;

       let%test_module "partial_eval" =
         (module struct
           let%test "partial_eval (Int 8) is (Int 8)" =
             partial_eval (Program ((), Int 8)) = Program ((), Int 8)
           ;;

           let%test "(+ (read) (- (+ 5 3))) compiles to (+ (read) -8)" =
             let initial_program =
               Program
                 ( (),
                   Prim
                     ( Add,
                       [
                         Prim (Read, []);
                         Prim (Subtract, [ Prim (Add, [ Int 5; Int 3 ]) ]);
                       ] ) )
             in
             let expected =
               Program ((), Prim (Add, [ Prim (Read, []); Int (-8) ]))
             in
             let actual = partial_eval initial_program in
             expected = actual
           ;;

           let%test "interp (program () (+ 10 (- (+ 5 3)))) = interp \
                     (partial_eval (program () (+ 10 (- (+ 5 3)))))" =
             let program =
               Program
                 ( (),
                   Prim
                     ( Add,
                       [ Int 10; Prim (Subtract, [ Prim (Add, [ Int 5; Int 3 ]) ]) ]
                     ) )
             in
             interp program = interp (partial_eval program)
           ;;

           let%test "interp (program () (+ 1 (+ 3 1))) = interp (partial_eval \
                     (program () (+ 1 (+ 3 1)))" =
             let program =
               Program ((), Prim (Add, [ Int 1; Prim (Add, [ Int 3; Int 1 ]) ]))
             in
             interp program = interp (partial_eval program)
           ;;

           let%test "interp (program () (- (+ 3 (- 5)))) = interp (partial_eval \
                     (program () (- (+ 3 (- 5)))))" =
             let program =
               Program ((), Prim (Subtract, [ Prim (Add, [ Int 3; Int 5 ]) ]))
             in
             interp program = interp (partial_eval program)
           ;;
         end)
       ;;
     end)
   ;; *)
