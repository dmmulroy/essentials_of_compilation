open Base

type t = { input : string; position : int; ch : char option }

let make input =
  match String.length input > 0 with
  | true -> { input; position = 0; ch = Some (String.get input 0) }
  | false -> { input; position = 0; ch = None }
;;

let is_letter = Char.is_alpha
let is_digit = Char.is_digit
let is_whitespace = Char.is_whitespace

let advance lexer =
  let position = lexer.position + 1 in
  match position >= String.length lexer.input with
  | true -> { lexer with ch = None }
  | false ->
      { lexer with position; ch = Some (String.get lexer.input position) }
;;

let rec advance_while (lexer : t) ~f:(predicate : char -> bool) : t =
  match lexer.ch with
  | Some ch when predicate ch -> lexer |> advance |> advance_while ~f:predicate
  | _ -> lexer
;;

let take_while (lexer : t) ~f:(predicate : char -> bool) : t * Buffer.t =
  let rec loop char_buffer lexer =
    match lexer.ch with
    | Some ch when predicate ch ->
        Buffer.add_char char_buffer ch;
        lexer |> advance |> loop char_buffer
    | _ -> (lexer, char_buffer)
  in
  loop (Buffer.create (String.length lexer.input - lexer.position)) lexer
;;

let skip_whitespace = advance_while ~f:is_whitespace

let read_identifier lexer =
  let advanced_lexer, char_buffer = take_while lexer ~f:is_letter in
  (advanced_lexer, Token.identifier_of_string (Buffer.contents char_buffer))
;;

let next_token lexer =
  let open Token in
  match lexer.ch with
  | None -> (lexer, None)
  | Some ch ->
      let advanced_lexer, token =
        match ch with
        | '-' -> (advance lexer, Negate)
        | '+' -> (advance lexer, Add)
        | '[' -> (advance lexer, LBracket)
        | ']' -> (advance lexer, RBracket)
        | '(' -> (advance lexer, LParen)
        | ')' -> (advance lexer, RParen)
        | ch when is_letter ch -> read_identifier lexer
        | _ -> (advance lexer, Eof)
      in
      (advanced_lexer, Some token)
;;

let%test_module "lexer" =
  (module struct
    open Token

    let lex input =
      String.fold input
        ~init:(make input, [])
        ~f:(fun (lexer, tokens) _char ->
          let advanced_lexer, token_opt = next_token lexer in
          match token_opt with
          | Some token -> (advanced_lexer, token :: tokens)
          | None -> (advanced_lexer, tokens))
      |> snd |> List.rev
    ;;

    let%test_module "advance" =
      (module struct
        let%test "it should correctly tokenize input" =
          let input = "-+[]()" in
          let tokens = lex input in
          let expected = [ Negate; Add; LBracket; RBracket; LParen; RParen ] in
          List.equal Token.equal expected tokens
        ;;
      end)
    ;;

    let%test_module "advance_while" =
      (module struct
        let%test "it should advance while the predicate fn is true" =
          let input = "+++++++-)" in
          let lexer = make input in
          let lexer = lexer |> advance_while ~f:(fun ch -> Char.equal ch '+') in
          let expected = [ Negate; RParen ] in
          let actual =
            lex
              (String.sub lexer.input ~pos:lexer.position
                 ~len:(String.length lexer.input - lexer.position))
          in
          List.equal Token.equal expected actual
        ;;
      end)
    ;;

    let%test_module "skip_whitespace" =
      (module struct
        let%test "it skips whitespace" =
          let input = "       +" in
          let lexer = make input in
          let expected = Add in
          let actual =
            lexer |> skip_whitespace |> next_token |> snd |> Option.value_exn
          in
          Token.equal expected actual
        ;;
      end)
    ;;

    let%test_module "read_identifier" =
      (module struct
        let%test "it reads 'program' to the Program token" =
          let input = "program" in
          let lexer = make input in
          let expected = Program in
          let actual = lexer |> read_identifier |> snd in
          Token.equal expected actual
        ;;

        let%test "it reads 'read' to the Read token" =
          let input = "read" in
          let lexer = make input in
          let expected = Read in
          let actual = lexer |> read_identifier |> snd in
          Token.equal expected actual
        ;;

        let%test "it reads an unknown string into an Identifier token" =
          let input = "foobar" in
          let lexer = make input in
          let expected = Identifier "foobar" in
          let actual = lexer |> read_identifier |> snd in
          Token.equal expected actual
        ;;
      end)
    ;;
  end)
;;
