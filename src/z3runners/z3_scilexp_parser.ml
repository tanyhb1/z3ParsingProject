open Syntax
open ParserUtil
open RunnerUtil
open GlobalConfig
open PrettyPrinters
open Core
open Z3ParserIntRep
open EvalUtil
open Eval
open EvalTypeUtilities
open EvalSyntax
open Z3
module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep
  
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep


let gas_limit = Stdint.Uint64.of_int 2000

(* initialize z3 context and solver *)
let context = Z3.mk_context []
let solver = Z3.Solver.mk_solver context None


let () =
  let cli = parse_cli() in
  let filename = cli.input_file in
  match FrontEndParser.parse_file ScillaParser.exp_term filename with
  | Ok e ->
      (* Since this is not a contract, we have no in-contract lib defined. *)
      let clib = { TC.UntypedSyntax.lname = asId "dummy";
                   TC.UntypedSyntax.lentries = [] } in
      StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
      let lib_dirs = StdlibTracker.get_stdlib_dirs() in
      if lib_dirs = [] then stdlib_not_found_err ();
      (* Import all libraries in known stdlib paths. *)
      let elibs = import_all_libs lib_dirs in
      let envres = Eval.init_libraries (Some clib) elibs in
      let env, gas_remaining = 
        (match envres Eval.init_gas_kont gas_limit with
        | Ok (env', gas_remaining) -> env', gas_remaining
        | Error (err, gas_remaining) -> fatal_error_gas err gas_remaining)
      in
      let lib_fnames = List.map ~f:(fun (name, _) -> name) env in
      let res' = Eval.exp_eval_wrapper e env in
      let res = res' Eval.init_gas_kont gas_remaining in
      (match res with
       | Ok ((_, env), _) ->
           (* pre-processing the env for translation to z3 *)
           (* first we remove the unnecessary objects *)
           let env_filtered = filter_env env lib_fnames in
           (* then we translate it to a list of z3Statements, which is an intermediate type defined solely for processing z3 expressions *)
           let env_translated_as_z3 = translate env_filtered in
           (* finally, parse as a list of z3 expressions *)
           let z3_expr_lst = z3unwrap env_translated_as_z3 context in
           (* add to our solver *)
           Z3.Solver.add solver z3_expr_lst
      | Error (el, gas_remaining) -> fatal_error_gas el gas_remaining)
  | Error e -> fatal_error e

(* run z3 solver, check sat, and get model *)
let () =
    match Z3.Solver.check solver [] with
    | UNSATISFIABLE -> Printf.printf "unsat\n"
    | UNKNOWN -> Printf.printf "unknown"
    | SATISFIABLE ->
        match Z3.Solver.get_model solver with
        | None -> ()
        | Some model ->
            Printf.printf "%s\n"
                (Z3.Model.to_string model)


