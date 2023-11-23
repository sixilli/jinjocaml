let read_file file = In_channel.with_open_text file In_channel.input_all

let main =
  let open Jinja_parser in
  let file = read_file "test.html" in
  let lexer = Lexer.init file in
  let parser = Parser.init lexer Parser.Context.make in
  match Parser.parse parser with
  | Ok v -> print_endline @@ Ast.show_node v
  | Error e -> print_endline @@ Parser.show_parse_error e
;;

let () = main
