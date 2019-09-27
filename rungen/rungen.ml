open Sexplib0

type wrapper = {name: string; command: string}

type run = {params: string}

type benchmark = {executable: string; name: string; runs: run list}

let space_regexp = Str.regexp "[ ]+"

let output_regexp = Str.regexp "%{output}"

let command_regexp = Str.regexp "%{command}"

let replace_spaces_with_periods str = Str.global_replace space_regexp "." str

let parse_json () =
  let open Yojson.Basic.Util in
  let to_run json = {params= json |> member "params" |> to_string} in
  let to_wrapper json =
    { name= json |> member "name" |> to_string
    ; command= json |> member "command" |> to_string }
  in
  let to_benchmark json =
    { executable= json |> member "executable" |> to_string
    ; name= json |> member "name" |> to_string
    ; runs= json |> member "runs" |> convert_each to_run }
  in
  let create_run (wrapper : wrapper) benchmark run =
    let run_params = replace_spaces_with_periods run.params in
    let run_name =
      Printf.sprintf "%s.%s.%s.bench" wrapper.name benchmark.name run_params
    in
    let subst_wrapper =
      Str.global_replace command_regexp
        (benchmark.executable ^ " " ^ run.params)
        (Str.global_replace output_regexp run_name wrapper.command)
    in
    let make_sexp_params_list =
      List.map (fun s -> Sexp.Atom s) (Str.split space_regexp subst_wrapper)
    in
    ( (wrapper, run_name)
    , Sexp.List
        [ Sexp.Atom "rule"
        ; Sexp.List [Sexp.Atom "targets"; Sexp.Atom run_name]
        ; Sexp.List
            [ Sexp.Atom "action"
            ; Sexp.List ([Sexp.Atom "run"] @ make_sexp_params_list) ] ] )
  in
  let config_json = Yojson.Basic.from_file "run_config.json" in
  let wrappers = config_json |> member "wrappers" |> convert_each to_wrapper in
  let benchmarks =
    config_json |> member "benchmarks" |> convert_each to_benchmark
  in
  let wrapper_run_names, run_sexps =
    List.split
      (List.flatten
         (List.flatten
            (List.map
               (fun wrapper ->
                 List.map
                   (fun benchmark ->
                     List.map
                       (fun run -> create_run wrapper benchmark run)
                       benchmark.runs )
                   benchmarks )
               wrappers)))
  in
  run_sexps
  @ List.map
      (fun (wrapper : wrapper) ->
        Sexp.List
          [ Sexp.Atom "alias"
          ; Sexp.List [Sexp.Atom "name"; Sexp.Atom ("run_" ^ wrapper.name)]
          ; Sexp.List
              ( [Sexp.Atom "deps"]
              @ List.map
                  (fun (_, x) -> Sexp.Atom x)
                  (List.filter
                     (fun (w, _) -> if w == wrapper then true else false)
                     wrapper_run_names) ) ] )
      wrappers

let () =
  print_endline
    (String.concat "\n\n" (List.map Sexp.to_string_hum (parse_json ())))