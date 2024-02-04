open Eoc.Rint

let ast1_1 = Prim (Add, [ Prim (Read, []); Prim (Negate, [ Int 8 ]) ])
let _ = interp (Program ((), ast1_1)) |> print_int
