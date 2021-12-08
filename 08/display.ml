open Printf
open Toolbox
open Toolbox.Operators


let parse_input =
    String.split_on_char '|'
        >> List.map String.trim
        >> List.map (String.split_on_char ' ')
        >> peek (fun l -> assert(List.length l == 2))
        >> fork List.hd (List.tl >> List.hd)


let count_unique =
    let is_unique str =
        let len = String.length str in
        List.exists ((==) len) [2; 3; 4; 7] in
    List.filter is_unique >> List.length


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    File.as_seq Sys.argv.(1)
        |> Seq.map parse_input
        |> Seq.map (second count_unique)
        |> Seq.fold_left (fun a (_, c) -> a + c) 0
        |> printf "%d\n"
