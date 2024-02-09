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
  let lexer, token = Lexer.next_token parser.lexer in
  match token with
  | Add -> (advance parser, Ast.Add)
  | Subtract -> (advance parser, Ast.Subtract)
  | Read -> (advance parser, Ast.Read)
  | _ ->
      raise
        (Invalid_token
           ( token,
             Fmt.str "Expected Add, Subtract, or Read. Received %a" Token.pp
               token ))
;;

let token_is_operation = function
  | Token.Read -> true
  | Token.Add -> true
  | Token.Subtract -> true
  | _ -> false
;;

let rec parse_expression (parser : t) =
  match peek parser with
  | Int int -> (advance parser, Ast.Int int)
  | LParen ->
      let parser', operation = parse_operation (advance parser) in
      let rec parse_arguments parser expressions =
        match peek parser with
        | RParen -> (advance parser, List.rev expressions)
        | _ as parser' ->
            let parser', expression = parse_expression (advance parser) in
            parse_arguments (advance parser') (expression :: expressions)
      in
      let parser', arguments = parse_arguments parser' [] in
      (parser', Ast.Prim { operation; expressions = arguments })
  | token ->
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
