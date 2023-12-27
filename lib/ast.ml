type node =
  | Program of program
  | Expression of expression
  | Statement of statement

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | String of string (** Token.T*)
  | List of string list
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : Token.t (** Token.T*)
      ; right : expression
      }
  | If of
      { condition : expression
      ; consequence : block
      ; alternative : block option
      }
  | FunctionLiteral of
      { parameters : identifier list
      ; body : block
      }
  | Call of
      { fn : expression
      ; args : expression list
      }
  | Array of expression list
  | Index of
      { left : expression
      ; index : expression
      }
  | Hash of (expression * expression) list
  | Macro of
      { parameters : identifier list
      ; body : block
      }
[@@deriving show { with_path = false }, sexp]

and statement =
  | Variable of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block
[@@deriving show { with_path = false }, sexp]

and identifier = { identifier : string } [@@deriving sexp]
and block = { block : statement list }
and program = { statements : statement list }

and whiteSpaceStrip =
  { left : bool
  ; right : bool
  }

let token_literal = function
  | Program _ -> "program"
  | Expression _ -> "expression"
  | Statement _ -> "statement"
;;

let variable_to_string v =
  match v with
  | String s -> s
  | _ -> failwith "unexpected non string statement"
;;

let build_template node =
  let rec build_string nodes str =
    match nodes with
    | [] -> str
    | hd :: tl ->
      (match hd with
       | ExpressionStatement stmnt ->
         (match stmnt with
          | String s -> build_string tl (str ^ s)
          | _ -> build_string tl (str ^ ":|"))
       | Variable v ->
         let var_as_str = variable_to_string v.value in
         build_string tl (str ^ var_as_str ^ "\n")
       | _ -> build_string tl (str ^ ":("))
  in
  match node with
  | Program n -> build_string n.statements ""
  | _ -> ":("
;;
