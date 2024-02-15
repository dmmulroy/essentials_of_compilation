module Todo = struct
  type t = { id : int; name : string; completed : bool }
  type t2 = { id : int; name : string; completed : bool }

  let make id name = { id; name; completed = false }
  let complete todo = { todo with completed = true }
  let to_string todo = Printf.sprintf "%d: %s" todo.id todo.name
end

let main () =
  let my_todo = Todo.make 1 "Learn OCaml & ReasonML" in
  let completed_todo = Todo.complete my_todo in
  Fmt.pr "%s\n" (Todo.to_string completed_todo)
;;
