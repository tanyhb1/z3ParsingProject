open Syntax
open Core
open ErrorUtils
open EvalUtil
open MonadUtil
open EvalMonad
open EvalMonad.Let_syntax
open PatternMatching
open Stdint
open ContractUtil
open PrettyPrinters
open Eval
open EvalTypeUtilities
open EvalSyntax
open Z3
module CU = ScillaContractUtil (ParserUtil.ParserRep) (ParserUtil.ParserRep)

type z3Literal =
  | String of string
  | Int of int_lit 
  | Uint of uint_lit
  | Bool of bool

type z3Map =
  | KV of (z3Literal * z3Literal) list
  | TypesKV of typ * typ
 
type z3Statements =
  | StringStmt of string * z3Literal
  | IntStmt of string * z3Literal
  | UintStmt of string * z3Literal
  | BoolStmt of string * z3Literal
  | ListStmt of string * z3Literal list
  | NatStmt of string * int
  | OpStmt of string * z3Literal option
  | MapStmt of string * z3Map * z3Map

let translate_typ_to_z3Literal i =
  match i with
  | PrimType x ->
      (match x with
      | Int_typ i' -> "Int" 
      | Uint_typ i' -> "Uint"
      | String_typ  -> "String"
      | _ -> raise (Failure "not implemented yet"))
  (* we only implement primtyps for maps so far, so no nesting of maps *)
  | _ -> raise (Failure "not implemented yet")
    
let translate_int_lit i =
  Int256L i

let translate_int_lit_to_int i =
  match i with
  | Int32L i' -> Int32.to_int i'
  | Int64L i' -> Int64.to_int i'
  | Int128L i' -> Int128.to_int i'
  | Int256L i'-> int_of_string @@ Integer256.Int256.to_string i'

let translate_uint_lit_to_int i =
  match i with
  | Uint32L i' -> Uint32.to_int i'
  | Uint64L i' -> Uint64.to_int i'
  | Uint128L i' -> Uint128.to_int i'
  | Uint256L i'-> int_of_string @@ Integer256.Uint256.to_string i'
    
let translate_literals lit =
  match lit with
  | StringLit s -> String s
  | IntLit i -> Int i
  | UintLit i -> Uint i
  | _ -> String "placeholder"

let translate_z3literals_to_symbols lit =
  match lit with
  | String s -> s
  | Int i -> string_of_int (translate_int_lit_to_int i)
  | Uint i -> string_of_int (translate_uint_lit_to_int i)
  | _ -> "Placeholder"
    
let filter_env env exclude_names =
  let lst = ref [] in
  let enames = List.append exclude_names Eval.reserved_names in
  let filter_prelude = fun (k, _) ->
    not (List.mem enames k ~equal:(fun s1 s2 -> s1 = s2))
  in
  List.iter env (fun f ->
      if filter_prelude f then (lst := f :: !lst;)
      else ());
  List.rev !lst;;

let rec translate env =
  match env with
  | h :: t -> 
      (let (lhs, rhs) = (fst h, snd h) in
      match rhs with
      | StringLit s -> StringStmt (lhs, String s) :: translate t
      | IntLit i -> IntStmt (lhs, Int i) :: translate t
      | UintLit i -> UintStmt (lhs, Uint i) :: translate t
      | ADTValue (cn, _, al) ->
          (match cn with
           | "True" -> BoolStmt (lhs, Bool true) ::translate t
           | "False" -> BoolStmt (lhs, Bool false) :: translate t
           | "Cons" ->
               let rec pcons largs =
                 if List.length largs = 0 then []
                 else
                   let this = translate_literals @@ List.nth_exn largs 0
                   in
                     match (List.nth_exn largs 1) with
                     | ADTValue(_,_, al') ->
                         this :: (pcons al')
                     | _ -> raise (Failure "malformed list")
               in
               ListStmt (lhs, pcons al) :: translate t
           | "Zero" | "Succ" ->
               let rec counter largs =
                 if List.length largs <> 1 then raise (Failure "malformed nat") else
                 if List.length largs = 0 then 0 else
                   (match (List.nth_exn largs 0) with
                    | ADTValue (_, _, al') ->
                        (counter al' + 1)
                    | _ -> raise (Failure "malformed nat")
                   )
               in
               NatStmt (lhs, (counter al)) :: translate t
           | "Some"  ->
               OpStmt (lhs, Some (translate_literals @@ List.nth_exn al 0)) :: translate t
           | "None" -> OpStmt (lhs, None) :: translate t
           | _ -> raise (Failure "Pairs/User-defined ADT will be implemented later")
          )
      | Map ((kt, vt), kv) ->
          let as_lst = (Caml.Hashtbl.fold (fun k v a ->
               (translate_literals k, translate_literals v) :: a
             )
               kv [])
            in
           MapStmt (lhs, TypesKV (kt, vt), KV as_lst) :: translate t
          
      | _ -> translate t
      )
  | [] -> [];;


(* let rec test (lst : string list) =
 *   let counter = ref 0 in
 *   match lst with
 *   | h :: t ->
 *       let (List.nth_exn lst !counter) =
 *         "asdfasdf" in
 *       counter := !counter + 1;
 *       test t
 *   | [] -> *)





(* Convert to expr list  *)
let rec z3unwrap (stmt_list : z3Statements list) (ctx : context) =
  match stmt_list with
  | z3stmt :: t ->
      (match z3stmt with
       | StringStmt ((lhs : string), rhs) -> z3unwrap t ctx
       | IntStmt ((lhs : string), rhs) ->
           let x = Z3.Arithmetic.Integer.mk_const_s ctx lhs in
           (match rhs with
           | Int i ->
               let y = Z3.Arithmetic.Integer.mk_numeral_i ctx (translate_int_lit_to_int i)
               in
               let z = Z3.Arithmetic.mk_ge ctx x y in
               let z' = Z3.Arithmetic.mk_le ctx x y in
               let expr = Z3.Boolean.mk_and ctx [z; z'] in
               expr :: z3unwrap t ctx
           | _ -> raise (Failure "malformed logical statement"))
       | UintStmt ((lhs : string), rhs) ->
           let x = Z3.Arithmetic.Integer.mk_const_s ctx lhs in
           (match rhs with
            | Uint i ->
               let y = Z3.Arithmetic.Integer.mk_numeral_i ctx (translate_uint_lit_to_int i)
               in
               let z = Z3.Arithmetic.mk_ge ctx x y in
               let z' = Z3.Arithmetic.mk_le ctx x y in
               let expr = Z3.Boolean.mk_and ctx [z; z'] in
               expr :: z3unwrap t ctx
            | _ -> raise (Failure "malformed logical statement"))
       | BoolStmt ((lhs : string), (rhs : z3Literal)) ->
           let x = Z3.Boolean.mk_const_s ctx lhs in
           let y = if rhs = Bool true then Z3.Boolean.mk_true ctx else Z3.Boolean.mk_false ctx in
           (* declares a Bool constant, and then checks that it is true/false depending on the previously assigned truth value *)
           (* ASSERT *)
           let z = Z3.Boolean.mk_and ctx [x;y] in
           z :: z3unwrap t ctx
       (* Inserting into a list isn't defined in Ocaml-z3 bindings, so have to omit *)
       | ListStmt ((lhs : string), rhs) -> z3unwrap t ctx
       | NatStmt ((lhs : string), rhs) ->
           let x = Z3.Arithmetic.Integer.mk_const_s ctx lhs in
           let y = Z3.Arithmetic.Integer.mk_numeral_i ctx rhs in
           let z = Z3.Arithmetic.mk_ge ctx x y in
           let z' = Z3.Arithmetic.mk_le ctx x y in
           let expr = Z3.Boolean.mk_and ctx [z; z'] in
           expr :: z3unwrap t ctx
       | OpStmt ((lhs : string), rhs) ->
           (match rhs with
           | Some e ->
               (match e with
               | String s -> z3unwrap t ctx        
               | Int i ->
                   let x = Z3.Arithmetic.Integer.mk_const_s ctx lhs in
                   let y = Z3.Arithmetic.Integer.mk_numeral_i ctx (translate_int_lit_to_int i)
                   in
                   let z = Z3.Arithmetic.mk_ge ctx x y in
                   let z' = Z3.Arithmetic.mk_le ctx x y in
                   let expr = Z3.Boolean.mk_and ctx [z; z'] in
                   expr :: z3unwrap t ctx
               | Uint i ->
                   let x = Z3.Arithmetic.Integer.mk_const_s ctx lhs in
                   let y = Z3.Arithmetic.Integer.mk_numeral_i ctx (translate_uint_lit_to_int i)
                   in
                   let z = Z3.Arithmetic.mk_ge ctx x y in
                   let z' = Z3.Arithmetic.mk_le ctx x y in
                   let expr = Z3.Boolean.mk_and ctx [z; z'] in
                   expr :: z3unwrap t ctx
               | Bool b ->
                   let x = Z3.Boolean.mk_const_s ctx lhs in
                   let y = if b = true then Z3.Boolean.mk_true ctx else Z3.Boolean.mk_false ctx in
                   let z = Z3.Boolean.mk_and ctx [x;y] in
                   z :: z3unwrap t ctx)
           | None ->
               (* think about how to implement None as grammar in parser *)
               z3unwrap t ctx
           )
       | MapStmt (lhs, kv_typ, kv_lst) ->
           match kv_typ with
           | TypesKV (x,y) ->
               let x = translate_typ_to_z3Literal x in
               let y = translate_typ_to_z3Literal y in
               let k_sort_ref = ref @@ Z3.Seq.mk_string_sort ctx in
               let v_sort_ref = ref @@ Z3.Seq.mk_string_sort ctx in
               (match (x,y) with
                | "String", "String" ->
                    let k_sort = Z3.Seq.mk_string_sort ctx in
                    let v_sort = Z3.Seq.mk_string_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | "String", _ ->
                    let k_sort = Z3.Seq.mk_string_sort ctx in
                    let v_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | "Int", "String" ->
                    let k_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    let v_sort = Z3.Seq.mk_string_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | "Int", _ ->
                    let k_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    let v_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | "Uint", "String" ->
                    let k_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    let v_sort = Z3.Seq.mk_string_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | "Uint", _ ->
                    let k_sort = Z3.Arithmetic.Integer.mk_sort ctx in
                    let v_sort = Z3.Seq.mk_string_sort ctx in
                    k_sort_ref := k_sort;
                    v_sort_ref := v_sort;
                | _, _ -> raise (Failure "not yet implemented")
               );
               let kset = ref @@ Z3.Set.mk_empty ctx !k_sort_ref in
               let vset = ref @@ Z3.Set.mk_empty ctx !v_sort_ref in
               (match kv_lst with
                | KV lst ->
                  let arr = List.to_array lst in
                  let len = Array.length arr in
                  let k_stor = ref [] in
                    let v_stor = ref [] in
                    for i = 0 to len - 1 do
                      let item = arr.(i) in
                      let x = fst item in
                      let y = snd item in
                      let x = Z3.Expr.mk_const_s ctx (translate_z3literals_to_symbols x) !k_sort_ref in
                      let y = Z3.Expr.mk_const_s ctx (translate_z3literals_to_symbols y) !v_sort_ref in
                      kset := Z3.Set.mk_set_add ctx !kset x;
                      vset := Z3.Set.mk_set_add ctx !vset y;
                      k_stor := !kset :: !k_stor;
                      v_stor := !vset :: !v_stor;
                    done;
                     z3unwrap t ctx         
                | _ -> raise (Failure "malformed pair")
               )            
           | _ -> raise (Failure "malformed pair")         
      )
  | [] -> []


