let main =
  let open Jinja_parser in
  let open Tester in
  let my_data = { embed = "magnets what are they?"; baz = false; hi = [ "1"; "2" ] } in
  match Templates.render_tester my_data with
  | Ok v -> print_endline v
  | Error e -> print_endline e
;;

let () = main
