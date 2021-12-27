open Printf
open Toolbox
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators

module Image = struct
    module Pixel = struct
        type t =
            | Dark
            | Light

        let of_char = function
            | '.' -> Dark
            | '#' -> Light
            | _ -> raise (Failure "Invalid pixel char")

        let to_char = function
            | Dark -> '.'
            | Light -> '#'

        let to_bit = function
            | Dark -> 0
            | Light -> 1

        let init = Dark
    end

    type pixel = Pixel.t
    type algorithm = pixel array

    module Canvas = struct
        include Toolbox.Matrix.Make(Pixel)

        let extend (m: m) (d: pixel) =
            let w = width m and h = height m in
            let copy_pixels (x, y) _ = match (x, y) with
                | x', _ when x' = 0 || x' > w -> d
                | _, y' when y' = 0 || y' > h -> d
                | _, _ -> m.(y - 1).(x - 1) in

            create (2 + width m) (2 + height m)
                |> mapi copy_pixels
    end

    type canvas = Canvas.m

    module Picture = struct
        type t = {
            canvas: canvas;
            field: pixel
        }

        let from_canvas c =
            { canvas = c; field = Pixel.Dark }

        let neighbours (x, y) =
            let offsets = Range.(make_inc (-1) (1) |> as_list) in

            List.product offsets offsets
                |> List.map (fun (dy, dx) -> (x + dx, y + dy))

        let enhance (a: algorithm) (p: t) =
            let build_index =
                let rec build_index' a = function
                    | [] -> a
                    | x::xs -> build_index' (a * 2 + x) xs in
                build_index' 0 in

            let q = { p with canvas = Canvas.extend p.canvas p.field } in

            let enhance_pixel pos _ =
                neighbours pos
                    |> List.map (Canvas.get_opt q.canvas
                            >> Option.value ~default:q.field)
                    |> List.map (Pixel.to_bit)
                    |> build_index
                    |> fun i -> a.(i) in

            let switch_field field =
                let value = List.replicate 9 (Pixel.to_bit field)
                        |> build_index in
                a.(value) in

            { canvas = Canvas.mapi enhance_pixel q.canvas;
                field = switch_field q.field }

        let repeat_enhance n a m =
            0 &-- n |> List.fold_left (fun m' _ -> enhance a m') m
    end

    type picture = Picture.t

    module Input = struct
        include Parser.CharParser

        let parse_algorithm =
            many1 (any_of "#.")
                |> map (List.map Pixel.of_char >> Array.of_list)
        let parse_image = sep_by eol (many1 (any_of "#."))
                |> map (List.map (List.map Pixel.of_char))
                |> map (Canvas.from_lists)

        let par =
            combine (parse_algorithm |> skip (many eol)) parse_image

        let parse: char Seq.t -> algorithm * canvas =
            Seq.resident >> run par () >> function
                | Ok v -> v
                | Error (s, _) -> raise (Failure s)
    end

    let count_light_pixels (p: picture) =
        Canvas.fold_left (fun a p -> a + (Pixel.to_bit p)) 0 p.canvas

    let print (p: picture) =
        let print_row row =
            printf "# |";
            Array.iter (Pixel.to_char >> printf "%c") row;
            printf "|\n" in
        printf "# --- Image [%d × %d] ---\n" (Canvas.width p.canvas) (Canvas.height p.canvas);
        Array.iter print_row p.canvas;
end


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc < 1 || argc > 2 then begin
        Printf.eprintf "usage: %s FILE [ENHANCEMENTS]\n" Sys.argv.(0);
        exit 1;
    end;

    let [@warning "-26"] count = if argc = 2
        then Num.parse_int_exn Sys.argv.(2)
        else 2 in

    Printexc.record_backtrace true;

    File.as_char_seq Sys.argv.(1)
        |> Image.Input.parse
        |> Pair.second Image.Picture.from_canvas
        |> Pair.uncurry (Image.Picture.repeat_enhance count)
        |> Fun.peek Image.print
        |> Image.count_light_pixels
        |> printf "%d\n"
