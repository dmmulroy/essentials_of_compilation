open Stdio

let rec print_tokens lexer =
  let open Token in
  let advanced_lexer, token = Lexer.next_token lexer in
  match token with
  | Eof -> ()
  | token ->
      Out_channel.printf "%s\n%!" (Fmt.str "%a" Token.pp token);
      print_tokens advanced_lexer
;;

let run () =
  Out_channel.print_endline "Lint REPL\n";
  let rec loop () =
    Out_channel.printf "Please enter a valid Lint expression: %!";
    let line_opt = In_channel.(stdin |> input_line) in
    match line_opt with
    | None -> ()
    | Some "" -> ()
    | Some line ->
        line |> Lexer.make |> Parser.make |> Parser.parse
        |> Interpreter.interpret
        |> Out_channel.printf "The result is: %d\n%!";
        loop ()
  in
  loop ()
;;
