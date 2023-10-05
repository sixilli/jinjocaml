open Base

let show_expression = Ast.show_expression
let ( let* ) res f = Base.Result.bind res ~f

type precedence =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  | `Index ]
[@@deriving show, ord]

let prec_gte a b = compare_precedence a b >= 0

let token_prec : Token.t -> precedence = function
  | Equal | NotEqual -> `Equals
  | LessThan | GreaterThan -> `LessGreater
  | Plus | Minus -> `Sum
  | Slash | Asterisk -> `Product
  | LeftParen -> `Call
  | LeftBracket -> `Index
  | _ -> `Lowest

type t = { lexer : Lexer.t; current : Token.t option; peek : Token.t option }
[@@deriving show]

type parse_error = { msg : string; parser : t; statements : Ast.statement list }
[@@deriving show]

let err parser msg statements = Error { parser; msg; statements }

let advance parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; peek; current = parser.peek }

let advance_until parser f =
  let parser = ref parser in
  while not (f !parser) do
    parser := advance !parser
  done;
  !parser

let chomp_semicolon parser =
  match parser.peek with Some Token.Semicolon -> advance parser | _ -> parser

let chomp_expression_end parser =
  match parser.peek with
  | Some Token.ExpressionEnd -> advance parser
  | _ -> parser

let next_token parser =
  let parser = advance parser in
  (parser, parser.current)

let expect_peek parser condition =
  match parser.peek with
  | Some token ->
      if condition token then Ok (advance parser)
      else Error (Fmt.failwith "missing peeked: %a" pp parser)
  | None -> Error "no peek token"

let peek_is parser token = Option.equal Token.equal parser.peek (Some token)

let expect_assign parser =
  expect_peek parser (function Token.Assign -> true | _ -> false)

let expect_expression_end parser =
  expect_peek parser (function Token.ExpressionEnd -> true | _ -> false)

let expect_colon parser =
  expect_peek parser (function Token.Colon -> true | _ -> false)

let expect_lparen parser =
  expect_peek parser (function Token.LeftParen -> true | _ -> false)

let expect_rparen parser =
  expect_peek parser (function Token.RightParen -> true | _ -> false)

let expect_lbrace parser =
  expect_peek parser (function Token.LeftBrace -> true | _ -> false)

let expect_rbrace parser =
  expect_peek parser (function Token.RightBrace -> true | _ -> false)

let expect_lbracket parser =
  expect_peek parser (function Token.LeftBracket -> true | _ -> false)

let expect_rbracket parser =
  expect_peek parser (function Token.RightBracket -> true | _ -> false)

let peek_precedence parser =
  match parser.peek with Some token -> token_prec token | _ -> `Lowest

let curr_precedence parser =
  match parser.current with Some token -> token_prec token | _ -> `Lowest

let init lexer =
  let parser = { lexer; current = None; peek = None } in
  let parser = advance parser in
  let parser = advance parser in
  parser

let rec parse parser =
  let rec parse' parser statements =
    match parser.current with
    | Some _ -> (
        match parse_statement parser with
        | Ok (parser, stmnt) -> parse' (advance parser) (stmnt :: statements)
        | Error msg -> err parser msg statements)
    | None -> Ok (parser, List.rev statements)
  in
  let* _, statements = parse' parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.current with
  | Some Token.Return -> parse_return parser
  | Some Token.Expression -> parse_jinja_expression parser
  | Some _ -> parse_expression_statement parser
  | None -> Error "no more tokens"

and parse_variable parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_expression_end parser in
  (* move parser onto the beginning of the expression *)
  Ok
    ( parser,
      Ast.Variable { name; value = Ast.String (Ast.show_identifier name) } )

and parse_return parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Return expr)

and parse_identifier parser =
  match parser.peek with
  | Some (Ident identifier) -> Ok (advance parser, { identifier })
  | _ -> Error "missing ident"

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.ExpressionStatement expr)

and parse_block parser =
  let parser = advance parser in
  let rec parse_block' parser statements =
    match parser.current with
    | Some Token.RightBrace -> Ok (parser, List.rev statements)
    | Some _ ->
        let* parser, statement = parse_statement parser in
        parse_block' (advance parser) (statement :: statements)
    | None -> Error "unexpected eof"
  in
  let* parser, block = parse_block' parser [] in
  Ok (parser, Ast.{ block })

and parse_jinja_expression parser =
  let* parser, name = parse_identifier parser in
  let* parser = expect_assign parser in
  (* move parser onto the beginning of the expression *)
  let parser = advance parser in
  let* parser, value = parse_expression parser `Lowest in
  let parser = chomp_semicolon parser in
  Ok (parser, Ast.Variable { name; value })

and parse_expression parser prec =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked = parser.peek |> Option.value ~default:Token.Illegal in
    let prec_peek = token_prec peeked in
    if peek_is parser Token.Semicolon || prec_gte prec prec_peek then
      Ok (parser, left)
    else
      match get_infix_fn parser with
      | Some infix_fn ->
          let parser = advance parser in
          let* parser, left = infix_fn parser left in
          parse_expression' parser left
      | None -> Ok (parser, left)
  in
  parse_expression' parser left

and parse_prefix_expression parser =
  let map_parser = Result.map ~f:(fun v -> (parser, v)) in
  let token = parser.current |> Option.value_exn in
  let () = Stdlib.print_endline @@ Token.show token in
  match token with
  | Token.Ident _ -> expr_parse_identifier parser |> map_parser
  | Token.Integer _ -> expr_parse_number parser |> map_parser
  | Token.String _ -> expr_parse_string parser |> map_parser
  | Token.Bang -> expr_parse_prefix parser token
  | Token.Minus -> expr_parse_prefix parser token
  | Token.True -> expr_parse_bool parser token
  | Token.False -> expr_parse_bool parser token
  | Token.LeftParen -> expr_parse_grouped parser
  | Token.If -> expr_parse_if parser
  | Token.Function -> expr_parse_fn parser
  | Token.Macro -> expr_parse_macro parser
  | Token.LeftBracket -> expr_parse_array_literal parser
  | Token.LeftBrace -> expr_parse_hash_literal parser
  | Token.LessThan -> expr_parse_identifier parser |> map_parser
  | Token.GreaterThan -> expr_parse_identifier parser |> map_parser
  | tok ->
      Error (Fmt.str "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and parse_infix_expression parser left =
  let operator = parser.current |> Option.value_exn in
  let prec = curr_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser prec in
  Ok (parser, Ast.Infix { left; operator; right })

and parse_call_expression parser fn =
  parse_list_of_exprs parser ~close:Token.RightParen ~final:(fun args ->
      Ast.Call { fn; args })

and parse_index_expression parser left =
  let parser = advance parser in
  let* parser, index = parse_expression parser `Lowest in
  let* parser = expect_rbracket parser in
  Ok (parser, Ast.Index { left; index })

and get_infix_fn parser =
  let open Token in
  match parser.peek with
  | Some Plus
  | Some Minus
  | Some Slash
  | Some Asterisk
  | Some Equal
  | Some NotEqual
  | Some LessThan
  | Some GreaterThan ->
      Some parse_infix_expression
  | Some LeftParen -> Some parse_call_expression
  | Some LeftBracket -> Some parse_index_expression
  | _ -> None

and expr_parse_identifier parser =
  match parser.current with
  | Some (Ident identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing number"

and expr_parse_string parser =
  match parser.current with
  | Some (String str) -> Ok (Ast.String str)
  | _ -> Error "missing string"

and expr_parse_number parser =
  match parser.current with
  | Some (Integer num) ->
      let num =
        try Int.of_string num
        with Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
      in
      Ok (Ast.Integer num)
  | _ -> Error "missing number"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Prefix in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_bool parser bool =
  let* bool =
    match bool with
    | Token.True -> Ok true
    | Token.False -> Ok false
    | _ -> Error "not a valid boolean"
  in
  Ok (parser, Ast.Boolean bool)

and expr_parse_grouped parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let* parser =
    expect_peek parser (function Token.RightParen -> true | _ -> false)
  in
  Ok (parser, expr)

and parse_list_of_exprs parser ~close ~final =
  let rec parse' parser exprs =
    match parser.peek with
    | Some tok when phys_equal close tok ->
        Ok (advance parser, final (List.rev exprs))
    | Some Token.Comma ->
        let parser = advance parser in
        let parser = advance parser in
        let* parser, expr = parse_expression parser `Lowest in
        parse' parser (expr :: exprs)
    | _ -> Error "unexpected next token"
  in
  match parser.peek with
  | Some tok when phys_equal close tok -> parse' parser []
  | Some _ ->
      let parser = advance parser in
      let* parser, expr = parse_expression parser `Lowest in
      parse' parser [ expr ]
  | None -> Error "hit eof"

and expr_parse_array_literal parser =
  parse_list_of_exprs parser ~close:Token.RightBracket ~final:(fun exprs ->
      Ast.Array exprs)

and expr_parse_hash_literal parser =
  let rec parse' parser exprs =
    let empty = List.length exprs = 0 in
    match parser.peek with
    | Some Token.RightBrace -> Ok (advance parser, Ast.Hash (List.rev exprs))
    | _ when empty -> parse_key_value parser exprs
    | Some Token.Comma when not empty -> parse_key_value (advance parser) exprs
    | _ -> Error "unexpected next token"
  and parse_key_value parser exprs =
    let parser = advance parser in
    let* parser, key = parse_expression parser `Lowest in
    let* parser = expect_colon parser in
    let parser = advance parser in
    let* parser, value = parse_expression parser `Lowest in
    parse' parser ((key, value) :: exprs)
  in
  parse' parser []

and expr_parse_if parser =
  let* parser = expect_lparen parser in
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_rparen parser in
  let* parser = expect_lbrace parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek with
    | Some Token.Else ->
        let parser = advance parser in
        let* parser = expect_lbrace parser in
        let* parser, block = parse_block parser in
        Ok (parser, Some block)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and read_identifier parser =
  match parser.current with
  | Some (Token.Ident identifier) -> Ok Ast.{ identifier }
  | _ -> Error "expected to read identifier"

and expr_parse_fn parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
        let parser = advance parser in
        let* identifier = read_identifier parser in
        parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })

and expr_parse_macro parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek with
    | Some Token.RightParen -> parse_list_of_parameters parser []
    | Some (Token.Ident _) ->
        let parser = advance parser in
        let* identifier = read_identifier parser in
        parse_list_of_parameters parser [ identifier ]
    | _ -> Error "unexpected start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.Macro { parameters; body })

and parse_list_of_parameters parser parameters =
  match parser.peek with
  | Some Token.RightParen -> Ok (advance parser, List.rev parameters)
  | Some Token.Comma ->
      let parser = advance parser in
      let parser = advance parser in
      let* ident = read_identifier parser in
      parse_list_of_parameters parser (ident :: parameters)
  | Some tok ->
      Error (Fmt.str "unexpected next parameter token %a" Token.pp tok)
  | None -> Error "unexpected end of stream"

let string_of_statement = function
  | Ast.Variable stmt ->
      Fmt.str "LET: let %s = %s"
        (Ast.show_identifier stmt.name)
        (show_expression stmt.value)
  | Return expr -> Fmt.str "RETURN %s" (show_expression expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (show_expression expr)
  | BlockStatement _ -> assert false

and string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
      Fmt.pr "Program: [@.";
      List.iter program.statements ~f:(fun s ->
          Fmt.pr "  %s@." (string_of_statement s));
      Fmt.pr "]@."
  | _ -> failwith "yaya"
