open Printf
open Toolbox.Core
open Toolbox.Parser

let () =
    let stream = "[2396,542,14]" |> String.to_chars |> List.to_seq in
    let par = CharParser.(between (symbol '[') (symbol ']') (sep_by (symbol ',') integer)) in
    match CharParser.run par () stream with
        | Some v -> printf "[ "; List.iter (printf "%d ")v ; printf "]\n";
        | None -> printf "Nothing\n"
