open Syntax
open ParserUtil
open RunnerUtil
open GlobalConfig
open PrettyPrinters
open Core
open Intermediate
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
           let env = filter_env env lib_fnames in
           List.iter env (fun f -> printf "%s, " (fst f); printf "%s \n" (pp_literal @@ snd f);)
      | Error (el, gas_remaining) -> fatal_error_gas el gas_remaining)
  | Error e -> fatal_error e
