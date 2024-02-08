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
let current_char { ch; _ } = ch

let advance lexer =
  let position = lexer.position + 1 in
  match position >= String.length lexer.input with
  | true -> { lexer with ch = None }
  | false ->
      { lexer with position; ch = Some (String.get lexer.input position) }
;;

let rec advance_while lexer ~f:predicate =
  match lexer.ch with
  | Some ch when predicate ch -> lexer |> advance |> advance_while ~f:predicate
  | _ -> lexer
;;

let skip_whitespace = advance_while ~f:is_whitespace
let peek lexer : char option = lexer |> advance |> current_char

let take_while lexer ~f:predicate =
  let rec loop char_buffer lexer =
    match lexer.ch with
    | Some ch when predicate ch ->
        Buffer.add_char char_buffer ch;
        lexer |> advance |> loop char_buffer
    | _ -> (lexer, char_buffer)
  in
  loop (Buffer.create (String.length lexer.input - lexer.position)) lexer
;;

let read_identifier lexer =
  let advanced_lexer, char_buffer = take_while lexer ~f:is_letter in
  (advanced_lexer, Token.identifier_of_string (Buffer.contents char_buffer))
;;

let read_digits lexer =
  let open Token in
  let advanced_lexer, char_buffer = take_while lexer ~f:is_digit in
  (advanced_lexer, Int (Buffer.contents char_buffer))
;;

let next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (lexer, Eof)
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
        | ch when is_digit ch -> read_digits lexer
        | ch -> (advance lexer, Illegal (Char.to_string ch))
      in
      (advanced_lexer, token)
;;

let%test_module "lexer" =
  (module struct
    open Token

    let lex input =
      let rec loop lexer tokens =
        let advanced_lexer, token = next_token lexer in
        match token with
        | Eof -> List.rev tokens
        | token -> loop advanced_lexer (token :: tokens)
      in
      loop (make input) []
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

    let%test_module "read_digits" =
      (module struct
        let%test "it reads digits successfully" =
          let input = "69" in
          let lexer = make input in
          let expected = Int "69" in
          let actual = lexer |> read_digits |> snd in
          Token.equal expected actual
        ;;

        let%test "it does read/lex decimals/floats" =
          let input = "6.9" in
          let lexer = make input in
          let expected = Int "6" in
          let actual = lexer |> read_digits |> snd in
          Token.equal expected actual
        ;;
      end)
    ;;

    let%test_module "skip_whitespace" =
      (module struct
        let%test "it skips whitespace" =
          let input = "       +" in
          let lexer = make input in
          let expected = Add in
          let actual = lexer |> skip_whitespace |> next_token |> snd in
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

    let%test_module "read_digits" =
      (module struct
        let%test "it reads digits successfully" =
          let input = "69" in
          let lexer = make input in
          let expected = Int "69" in
          let actual = lexer |> read_digits |> snd in
          Token.equal expected actual
        ;;

        let%test "it does read/lex decimals/floats" =
          let input = "6.9" in
          let lexer = make input in
          let expected = Int "6" in
          let actual = lexer |> read_digits |> snd in
          Token.equal expected actual
        ;;
      end)
    ;;

    let%test_module "peek" =
      (module struct
        let%test "it should return the next character" =
          let input = "69" in
          let lexer = make input in
          let peeked_char = peek lexer in
          let expected = Some '9' in
          Option.equal Char.equal expected peeked_char
        ;;

        let%test "it should return the next character" =
          let input = "a" in
          let lexer = make input in
          let peeked_char = peek lexer in
          let expected = None in
          Option.equal Char.equal expected peeked_char
        ;;
      end)
    ;;
  end)
;;
