open Ast
open Base
open Stdio

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
      Out_channel.print_string "Please enter an integer: ";
      match Stdlib.read_int_opt () with
      | Some n -> n
      | None -> raise (Invalid_read_operation "Read expected an integer"))
  | Prim { operation = Subtract; expressions = [ expression ] } ->
      -interpret_expression expression
  | Prim { operation = Add; expressions = [ expression_1; expression_2 ] } ->
      let a = interpret_expression expression_1 in
      let b = interpret_expression expression_2 in
      a + b
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

let partial_eval { body; info } = { info; body = partial_eval_expression body }

(* Tests *)
let%test_module "l_int" =
  (module struct
    let ast1_1_with_read_value rv =
      Prim
        {
          operation = Add;
          expressions =
            [ Int rv; Prim { operation = Subtract; expressions = [ Int 8 ] } ];
        }
    ;;

    let%test_module "is_leaf" =
      (module struct
        let%test "Read [] is a leaf" =
          Bool.equal
            (is_leaf (Prim { operation = Read; expressions = [] }))
            true
        ;;

        let%test "Int 8 is a leaf" = Bool.equal (is_leaf (Int 8)) true

        let%test "Subtract (Int 8) is not a leaf" =
          Bool.equal
            (is_leaf (Prim { operation = Subtract; expressions = [ Int 8 ] }))
            false
        ;;
      end)
    ;;

    let%test_module "interp_exp" =
      (module struct
        let%test "(+ 60 9) is 69" =
          interpret_expression
            (Prim { operation = Add; expressions = [ Int 60; Int 9 ] })
          = 69
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
                Prim
                  {
                    operation = Add;
                    expressions =
                      [
                        Prim { operation = Read; expressions = [] };
                        Prim
                          {
                            operation = Subtract;
                            expressions =
                              [
                                Prim
                                  {
                                    operation = Add;
                                    expressions = [ Int 5; Int 3 ];
                                  };
                              ];
                          };
                      ];
                  };
            }
          in
          let expected =
            {
              info = ();
              body =
                Prim
                  {
                    operation = Add;
                    expressions =
                      [ Prim { operation = Read; expressions = [] }; Int (-8) ];
                  };
            }
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
                Prim
                  {
                    operation = Add;
                    expressions =
                      [
                        Int 10;
                        Prim
                          {
                            operation = Subtract;
                            expressions =
                              [
                                Prim
                                  {
                                    operation = Add;
                                    expressions = [ Int 5; Int 3 ];
                                  };
                              ];
                          };
                      ];
                  };
            }
          in
          interpret program = interpret (partial_eval program)
        ;;

        let%test "interp (program () (+ 1 (+ 3 1))) = interp (partial_eval \
                  (program () (+ 1 (+ 3 1)))" =
          let program =
            {
              info = ();
              body =
                Prim
                  {
                    operation = Add;
                    expressions =
                      [
                        Int 1;
                        Prim { operation = Add; expressions = [ Int 3; Int 1 ] };
                      ];
                  };
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
                Prim
                  {
                    operation = Subtract;
                    expressions =
                      [
                        Prim { operation = Add; expressions = [ Int 3; Int 5 ] };
                      ];
                  };
            }
          in
          interpret program = interpret (partial_eval program)
        ;;
      end)
    ;;

    let%test "interpret (+ (10) (10)) is 20" =
      let program =
        {
          info = ();
          body = Prim { operation = Add; expressions = [ Int 10; Int 10 ] };
        }
      in
      let expected = 20 in
      let actual = interpret program in
      expected = actual
    ;;

    let%test "interpret (+ (10) (9)) is 19" =
      let program =
        {
          info = ();
          body = Prim { operation = Add; expressions = [ Int 10; Int 9 ] };
        }
      in
      let expected = 19 in
      let actual = interpret program in
      expected = actual
    ;;

    let%test "interpret (+ (69) (100)) is 169" =
      let program =
        {
          info = ();
          body = Prim { operation = Add; expressions = [ Int 69; Int 100 ] };
        }
      in
      let expected = 169 in
      let actual = interpret program in
      expected = actual
    ;;
  end)
;;
