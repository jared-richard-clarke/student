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

let rec deriv exp dv =
  match exp with
    Const c -> Const 0.0
  | Var v -> if v = dv then Const 1.0 else Const 0.0
  | Sum(f, g) -> Sum(deriv f dv, deriv g dv)
  | Diff(f, g) -> Diff(deriv f dv, deriv g dv)
  | Prod(f, g) -> Sum(Prod(f, deriv g dv), Prod(deriv f dv, g))
  | Quot(f, g) -> Quot(Diff(Prod(deriv f dv, g), Prod(f, deriv g dv)),
                       Prod(g, g)) 

(* 
   === Example: eval ===
   eval [("x", 2.0); ("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"));;
   - : float 7.14 
   
   === Example: deriv ===
   deriv (Quote(Const 1.0, Var "x")) "x";;
   - : expression = 
       Quot (Diff (Prod (Const 0.0, Var "x"), Prod (Const 1.0, Const 1.0)), Prod (Var "x", Var "x"))
*)
