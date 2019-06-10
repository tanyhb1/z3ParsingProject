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

(* 2. uninterpreted functions and constants, 
   i.e. sorts.

(declare-sort A)
(declare-const x A)
(declare-const y A)
(declare-fun f (A) A)
(assert (= (f (f x)) x))
(assert (= (f x) y))
(assert (not (= x y)))
(check-sat)
(get-model)

*)

let sort_test (ctx: context) =
  Printf.printf "Uninterpreted functions & constants test \n";
  let srt = (Sort.mk_uninterpreted_s ctx "s") in
  let x = (Expr.mk_const_s ctx "x" srt) in
  let y = (Expr.mk_const_s ctx "y" srt) in
  let domain = [srt;] in
  let f = (FuncDecl.mk_func_decl_s ctx "f" domain srt) in
  let fapp = (mk_app ctx f
                [x]) in
  let fapp' = (mk_app ctx f [fapp]) in
  let g = (mk_goal ctx true false false) in
  let assert1 = (Boolean.mk_eq ctx fapp' x) in
  let assert2 = (Boolean.mk_eq ctx fapp y) in
  let assert3 = (Boolean.mk_not ctx (Boolean.mk_eq ctx x y)) in
  (Goal.add g [assert1; assert2; assert3]);
  Printf.printf "%s \n" ("Goal: " ^ (Goal.to_string g));
  let solver = (mk_solver ctx None) in
  let form = get_formulas g in
  (List.iter (fun a -> (Solver.add solver [a])) form);
  (List.iter (fun a-> Printf.printf "Formula: %s \n" (Expr.to_string a)) form);
  match Solver.check solver [] with
  | UNSATISFIABLE -> Printf.printf "UNSATISFIABLE\n"
  | UNKNOWN -> Printf.printf "UNKNOWN\n"
  | SATISFIABLE ->
      match Solver.get_model solver with
      | None -> ()
      | Some model ->
          Printf.printf "%s\n" (Model.to_string model)
  

(* 3. universal quantification

(declare-const a Int)
(declare-const b Int)
(assert (forall ((t Int)) (=> (<= t a) (< t b))))
(check-sat-using (then qe smt))
(get-model)

*)

let uquant_test (ctx : context) =
  Printf.printf "Universal quantification test \n";
  let a = (Integer.mk_const_s ctx "a") in
  let b = (Integer.mk_const_s ctx "b") in
  let x = (Integer.mk_const_s ctx "x") in
  let eq1 = (Arithmetic.mk_lt ctx x b) in
  let eq2 = (Arithmetic.mk_le ctx x a) in
  let eq3 = (Boolean.mk_and ctx [eq1; eq2]) in
  let q = (Quantifier.mk_forall ctx [Integer.mk_sort ctx] [Symbol.mk_string ctx "x"]  (eq3) (Some 1) [] [] (Some (Symbol.mk_string ctx "Q1")) (Some (Symbol.mk_string ctx "skid1"))) in
  Printf.printf "%s \n" ("Goal: " ^ (Quantifier.to_string q));
  let solver = (mk_solver ctx None) in
  let expr_of_q = Quantifier.expr_of_quantifier q in
  Solver.add solver [expr_of_q];
  match Solver.check solver [] with
  | UNSATISFIABLE -> Printf.printf "UNSATISFIABLE\n"
  | UNKNOWN -> Printf.printf "UNKNOWN\n"
  | SATISFIABLE ->
      match Solver.get_model solver with
      | None -> ()
      | Some model ->
          Printf.printf "%s\n" (Model.to_string model)
  
  

(* running the functions *)
            
let () =
  try (
    if not (Log.open_ "z3.log") then
      raise (TestFailedException "Log couldn't be opened.")
    else
      (
        let cfg = [("model", "true"); ("proof", "false")] in
        let ctx = (mk_context cfg) in
        demorgan_test ctx;
        Printf.printf "\n";
        sort_test ctx;
        Printf.printf "\n";
        uquant_test ctx;
        (* perform garbage collection *)
       	Printf.printf "Disposing...\n";
	Gc.full_major ()
      );
    Printf.printf "Exiting. \n";
    exit 0
  ) with Error(msg) -> (
      Printf.printf "Error: %s \n" msg;
      exit 1
    );;

