(* 
   === Symbolic Processing of Expressions === 
   Example pulled from [The OCaml Manual](https://v2.ocaml.org/releases/4.14/htmlman/coreexamples.html)
   I created a similar application in Scheme, so this example teaches me a lot about OCaml's syntax.
*)

type expression =
    Const of float
  | Var of string
  | Sum of expression * expression    (* e1 + e2 *)
  | Diff of expression * expression   (* e1 - e2 *)
  | Prod of expression * expression   (* e1 * e2 *)
  | Quot of expression * expression   (* e1 / e2 *)
                                      
exception Unbound_variable of string
    
let rec eval env exp =
  match exp with
    Const c -> c
  | Var v ->
      (try List.assoc v env with Not_found -> raise (Unbound_variable v))
  | Sum(f, g) -> eval env f +. eval env g
  | Diff(f, g) -> eval env f -. eval env g
  | Prod(f, g) -> eval env f *. eval env g
  | Quot(f, g) -> eval env f /. eval env g
                    
(* 
   === Example ===
   eval [("x", 2.0); ("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"))
   - : float 7.14 
*)
