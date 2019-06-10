open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector

exception TestFailedException of string
(* exercise in translation from Z3 statements to OCaml-Z3 bindings *)
(* 1. demorgan's 
(declare-const a Bool)
(declare-const b Bool)
(define-fun demorgan () Bool
    (= (and a b) (not (or (not a) (not b)))))
(assert (not demorgan))
(check-sat)
*)

let demorgan_test (ctx : context) =
  Printf.printf "De Morgan's Test \n";
  let x = (mk_string ctx "x") in
  let y = (mk_string ctx "y") in
  let x = (Boolean.mk_const ctx x) in
  let y = (Boolean.mk_const ctx y) in
  let f = (Boolean.mk_eq ctx (Boolean.mk_and ctx [x; y] ) (Boolean.mk_not ctx (Boolean.mk_or ctx [(Boolean.mk_not ctx (x)); (Boolean.mk_not ctx (y))])) ) in
  let f_negation = Boolean.mk_not ctx f in
  Printf.printf "%s\n" ("Goal: " ^ (Expr.to_string f_negation));
  let solver = (mk_solver ctx None) in
  Solver.add solver [f_negation];
  match Solver.check solver [] with
  | UNSATISFIABLE -> Printf.printf "UNSATISFIABLE (Which means De Morgan's holds, since we entered the negation of it)\n"
  | UNKNOWN -> Printf.printf "UNKNOWN\n"
  | SATISFIABLE ->
      match Solver.get_model solver with
      | None -> ()
      | Some model ->
          Printf.printf "%s\n"
            (Model.to_string model)
            
  
let () =
  try (
    if not (Log.open_ "z3.log") then
      raise (TestFailedException "Log couldn't be opened.")
    else
      (
        let cfg = [("model", "true"); ("proof", "false")] in
        let ctx = (mk_context cfg) in
        demorgan_test ctx;
        (* perform garbage collection *)
       	Printf.printf "Disposing...\n";
	Gc.full_major ()
      );
    Printf.printf "Exiting. \n";
    exit 0
  ) with Error(msg) -> (
      Printf.printf "Error: %s \n" msg;
      exit 1
    )
