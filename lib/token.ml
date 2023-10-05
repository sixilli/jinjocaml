type t =
  | Illegal
  (* Items *)
  | Ident of string
  | Integer of string
  | String of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | Percent
  (* Delimiters *)
  | Comma
  | Semicolon
  | Colon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | StatementEnd
  | ExpressionEnd
  (* Keyword *)
  | Macro
  | Function
  | Variable
  | Statement
  | Expression
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq, sexp]

let lookup_ident str =
  match str with
  | "fn" -> Function
  | "variable" -> Variable
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | "macro" -> Macro
  | _ -> Ident str
