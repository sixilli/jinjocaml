type tester_template =
  { embed : string
  ; baz : bool
  ; hi : string list
  }

module Templates : sig
  val render_tester : tester_template -> (string, string) result
end = struct
  let tester_file = Loader.read_file "./test.html"

  let render_tester tmpl =
    let ctx = Parser.Context.make in
    let () =
      Base.Hashtbl.set ctx ~key:"embed" ~data:(Parser.Context.String tmpl.embed);
      Base.Hashtbl.set ctx ~key:"baz" ~data:(Parser.Context.Bool tmpl.baz);
      Base.Hashtbl.set ctx ~key:"hi" ~data:(Parser.Context.List tmpl.hi)
    in
    let parser = Parser.init (Lexer.init tester_file) ctx in
    match Parser.parse parser with
    | Ok v -> Ok (Ast.build_template v)
    | Error e -> Error (Parser.show_parse_error e)
  ;;
end

(* match parser with *)
(* | Ok n -> Ast.build_template n *)
(* | Error e -> failwith e *)
