open Base

type t = { input : string; position : int; ch : char option } [@@deriving show]

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let rec next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> (lexer, None)
  | Some ch ->
      let lexer, token =
        match ch with
        | ';' -> (advance lexer, Semicolon)
        | ch -> Fmt.failwith "unknown char: %c" ch
      in
      (lexer, Some token)

and advance lexer =
  if lexer.position >= String.length lexer.input - 1 then
    { lexer with ch = None }
  else
    let position = lexer.position + 1 in
    { lexer with position; ch = Some (String.get lexer.input position) }

and skip_whitespace lexer =
  let lexer, _ =
    seek lexer (fun ch ->
        match ch with Some ch -> Char.is_whitespace ch | None -> false)
  in
  lexer

and seek lexer condition =
  let rec loop lexer =
    if condition lexer.ch then loop @@ advance lexer else lexer
  in
  let lexer = loop lexer in
  (lexer, lexer.position)
