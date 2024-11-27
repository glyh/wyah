let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
      callback v;
      user_input prompt callback

let () = Wyah_compile.Entry.compile_pipeline |> user_input "Wyah> "
