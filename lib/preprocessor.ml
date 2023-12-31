(* Schema for code to be created *)

(*
   Steps below should be completed for each parsed template

type foo_ctx = { bar : string; baz : bool; hi : string list }

let render_foo (ctx: foo_context) : string 
  create new map from foo_ctx
*)

(**
[
  structure_item (./lib/tester.ml[1,0+0]..[5,78+3])
    Pstr_type Rec
    [
      type_declaration "tester_template" (./lib/tester.ml[1,0+5]..[1,0+20]) (./lib/tester.ml[1,0+0]..[5,78+3])
        ptype_params =
          []
        ptype_cstrs =
          []
        ptype_kind =
          Ptype_record
            [
              "embed" (./lib/tester.ml[2,23+4]..[2,23+9])                core_type (./lib/tester.ml[2,23+12]..[2,23+18])
                Ptyp_constr "string" (./lib/tester.ml[2,23+12]..[2,23+18])
              "baz" (./lib/tester.ml[3,42+4]..[3,42+7])                core_type (./lib/tester.ml[3,42+10]..[3,42+14])
                Ptyp_constr "bool" (./lib/tester.ml[3,42+10]..[3,42+14])
              "hi" (./lib/tester.ml[4,57+4]..[4,57+6])                core_type (./lib/tester.ml[4,57+9]..[4,57+20])
                Ptyp_constr "list" (./lib/tester.ml[4,57+16]..[4,57+20])
                [
                  core_type (./lib/tester.ml[4,57+9]..[4,57+15])
                    Ptyp_constr "string" (./lib/tester.ml[4,57+9]..[4,57+15])
                ]
            ]
        ptype_private = Public
        ptype_manifest =
          None
    ]
**)
open Ppxlib

let template_builder typ =
  let loc = typ.ptype_loc in
  [%expr fun i -> i]
;;

(** Steps to be take
    Generate struct

    **)

let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let files = Loader.load_templates "." in
  match files with
  | [] -> Location.raise_errorf ~loc "Could not find any html files"
  | hd :: tl -> [%stri let [%p Pat.var func_name] = [%e stringify]]
;;

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let deriver = Deriving.add "load_templates" ~str_type_decl:impl_generator
