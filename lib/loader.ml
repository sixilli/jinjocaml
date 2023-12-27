let read_file file = In_channel.with_open_text file In_channel.input_all

let load_templates path ext =
  let rec find_files ext dirs files_acc =
    match dirs with
    | [] -> files_acc
    | hd :: tl ->
      let new_dirs, new_files =
        Sys.readdir hd
        |> Array.to_list
        |> List.map (fun item -> hd ^ "/" ^ item)
        |> List.partition Sys.is_directory
      in
      let matched_files =
        List.filter (fun file -> Filename.extension file = ext) new_files
      in
      (match matched_files with
       | [] -> find_files ext (new_dirs @ tl) files_acc
       | _ -> find_files ext (new_dirs @ tl) (matched_files @ files_acc))
  in
  let files = find_files ext [ path ] [] in
  List.map read_file files
;;
