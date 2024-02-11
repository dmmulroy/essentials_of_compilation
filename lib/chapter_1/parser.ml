open Base

[@@@ocaml.warning "-32-27-26-39"]

type t = { lexer : Lexer.t; token : Token.t }

exception Invalid_token of Token.t * string

let make lexer =
  let lexer, token = Lexer.next_token lexer in
  { lexer; token }
;;

let advance parser =
  let lexer, token = Lexer.next_token parser.lexer in
  { lexer; token }
;;

let peek t = Lexer.next_token t.lexer |> snd

let parse_operation (parser : t) : t * Ast.operation =
  match parser.token with
  | Add -> (advance parser, Ast.Add)
  | Subtract -> (advance parser, Ast.Subtract)
  | Read -> (advance parser, Ast.Read)
  | _ ->
      raise
        (Invalid_token
           ( parser.token,
             Fmt.str "Expected Add, Subtract, or Read. Received %a\n\n" Token.pp
               parser.token ))
;;

let rec parse_expression parser =
  match (parser.token, peek parser) with
  | Int int, _ -> (advance parser, Ast.Int int)
  | LParen, Int _ | LParen, LParen ->
      let parser', expression = parser |> advance |> parse_expression in
      (advance parser', expression)
  | LParen, _ ->
      let parser', operation = parse_operation (advance parser) in
      let rec parse_arguments parser =
        match parser.token with
        | RParen -> (parser, [])
        | token ->
            let parser', expression = parse_expression parser in
            let parser', arguments = parse_arguments parser' in
            (parser', expression :: arguments)
      in
      let parser', arguments = parse_arguments parser' in
      (advance parser', Ast.Prim { operation; expressions = arguments })
  | token, _ ->
      raise
        (Invalid_token
           ( token,
             Fmt.str "Expected LParen or Int(int). Received %a" Token.pp token
           ))
;;

let parse (parser : t) : Ast.t =
  let _, body = parse_expression parser in
  { info = (); body }
;;

let%test_module "parser" =
  (module struct
    open Ast
    (*
    let%test "simple" =
      let input = "(+ (read) 3)" in
      let expected =
        {
          info = ();
          body =
            Prim
              {
                operation = Add;
                expressions =
                  [ Prim { operation = Read; expressions = [] }; Int 3 ];
              };
        }
      in
      let parser = make (Lexer.make input) in
      let actual = parse parser in
      Ast.equal expected actual
    ;; *)

    (* let%test "it parses ((10))" = *)
    (*   let input = "((10))" in *)
    (*   let expected = { info = (); body = Int 10 } in *)
    (*   let parser = make (Lexer.make input) in *)
    (*   let actual = parse parser in *)
    (*   Ast.equal expected actual *)
    (* ;; *)
    (**)
    (* let%test "it parses (+ (10) (10))" = *)
    (*   let input = "(+ (10) (10))" in *)
    (*   let expected = *)
    (*     { *)
    (*       info = (); *)
    (*       body = Prim { operation = Add; expressions = [ Int 10; Int 10 ] }; *)
    (*     } *)
    (*   in *)
    (*   let parser = make (Lexer.make input) in *)
    (*   let actual = parse parser in *)
    (*   Ast.equal expected actual *)
    (* ;; *)

    let%test "it parses (+ (10) (9))" =
      let input = "(+ (10) (9))" in
      let expected =
        {
          info = ();
          body = Prim { operation = Add; expressions = [ Int 10; Int 9 ] };
        }
      in
      let parser = make (Lexer.make input) in
      let actual = parse parser in
      Fmt.pr "Expected: %s\n" Ast.(to_string expected);
      Fmt.pr "Actual %s\n" Ast.(to_string actual);
      Ast.equal expected actual
    ;;

    (*
       let%test "it parses (+ (69) (100))" =
         let input = "(+ (69) (100))" in
         let expected =
           {
             info = ();
             body = Prim { operation = Add; expressions = [ Int 69; Int 100 ] };
           }
         in
         let parser = make (Lexer.make input) in
         let actual = parse parser in
         Ast.equal expected actual
       ;;

       let%test "complex" =
         let input = "(+ (read) (- (+ 5 3)))" in
         let expected =
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
         let parser = make (Lexer.make input) in
         let actual = parse parser in
         Ast.equal expected actual
       ;; *)
  end)
;;
