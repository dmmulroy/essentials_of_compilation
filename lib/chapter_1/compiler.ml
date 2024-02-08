type rint = Program of unit * exp
and exp = Int of int | Prim of op * exp list
and op = Read | Negate | Add

type exn += Invalid_expression of exp | Invalid_read_op of string

let is_leaf = function
  | Int _ -> true
  | Prim (Read, []) -> true
  | Prim (Negate, [ _ ]) -> false
  | Prim (Add, [ _; _ ]) -> false
  | exp -> raise (Invalid_expression exp)
;;

let rec is_exp = function
  | Int _ -> true
  | Prim (Read, []) -> true
  | Prim (Negate, [ exp ]) -> is_exp exp
  | Prim (Add, [ exp_1; exp_2 ]) -> is_exp exp_1 && is_exp exp_2
  | _ -> false
;;

let is_rint (Program (_, exp)) = is_exp exp

let rec interp_exp = function
  | Int n -> n
  | Prim (Read, []) -> (
      match read_int_opt () with
      | Some n -> n
      | None -> raise (Invalid_read_op "Read expected an integer"))
  | Prim (Negate, [ exp ]) -> -interp_exp exp
  | Prim (Add, [ exp_1; exp_2 ]) -> interp_exp exp_1 + interp_exp exp_2
  | exp -> raise (Invalid_expression exp)
;;

let interp (Program ((), exp)) = interp_exp exp

let partial_eval_negate = function
  | Int n -> Int (-n)
  | _ as exp -> Prim (Negate, [ exp ])
;;

let partial_eval_add = function
  | Int n, Int m -> Int (n + m)
  | exp_1, exp_2 -> Prim (Add, [ exp_1; exp_2 ])
;;

let rec partial_eval_exp = function
  | Int n -> Int n
  | Prim (Read, []) -> Prim (Read, [])
  | Prim (Negate, [ exp ]) -> partial_eval_negate (partial_eval_exp exp)
  | Prim (Add, [ exp_1; exp_2 ]) ->
      partial_eval_add (partial_eval_exp exp_1, partial_eval_exp exp_2)
  | exp -> raise (Invalid_expression exp)
;;

let partial_eval (Program ((), exp)) = Program ((), partial_eval_exp exp)

(* Tests *)
let%test_module "rint" =
  (module struct
    let ast1_1 = Prim (Add, [ Prim (Read, []); Prim (Negate, [ Int 8 ]) ])

    let ast1_1_with_read_value rv =
      Prim (Add, [ Int rv; Prim (Negate, [ Int 8 ]) ])
    ;;

    let%test_module "is_leaf" =
      (module struct
        let%test "Read [] is a leaf" = is_leaf (Prim (Read, [])) = true
        let%test "Int 8 is a leaf" = is_leaf (Int 8) = true

        let%test "Negate (Int 8) is not a leaf" =
          is_leaf (Prim (Negate, [ Int 8 ])) = false
        ;;
      end)
    ;;

    let%test_module "is_rint" =
      (module struct
        let%test "ast1_1 is a valid rint program" =
          is_rint (Program ((), ast1_1)) = true
        ;;

        let invalid_program =
          Program ((), Prim (Negate, [ Prim (Read, []); Prim (Add, [ Int 8 ]) ]))
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
                      Prim (Negate, [ Prim (Add, [ Int 5; Int 3 ]) ]);
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
                    [ Int 10; Prim (Negate, [ Prim (Add, [ Int 5; Int 3 ]) ]) ]
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
            Program ((), Prim (Negate, [ Prim (Add, [ Int 3; Int 5 ]) ]))
          in
          interp program = interp (partial_eval program)
        ;;
      end)
    ;;
  end)
;;
