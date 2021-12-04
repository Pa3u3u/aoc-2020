open Printf
open Toolbox.Operators


exception Invalid_input of string

module RawBoard = struct
    type board = int list list


    let validate (b : board) =
        let outer_length = List.length b in
            List.for_all (List.length >> ((==) outer_length)) b


    let empty (b : board) =
        List.length b == 0


    let size (b : board) =
        if not (validate b) then
            raise (Invalid_input "Cannot get dimensions of invalid board")
        else if empty b then
            (0, 0)
        else
            Toolbox.fork (List.length) (List.hd >> List.length) b


    let of_seq input =
        let parse_line =
            String.split_on_char ' ' >> List.filter_map Toolbox.Num.parse_int in

        let read_line input = match input () with
            | Seq.Nil -> ([], Seq.Nil)
            | Seq.Cons (line, rest) -> (parse_line line, rest ()) in

        let append (board : board) (row : int list) =
            List.append board [row] in

        let rec ctor (board : board) input = match read_line input with
            | ([], rest) when empty board -> (None, rest)
            | ([], rest) -> (Some board, rest)
            | (row, Seq.Nil) -> (Some (append board row), Seq.Nil)
            | (row, rest) -> ctor (append board row) (fun _ -> rest) in

        ctor [] input
end


module Board = struct
    type cell = { value: int; mutable mark: bool }
    type board = cell array array


    let empty_cell = { value = 0; mark = false }


    let from_raw (b : RawBoard.board) =
        let copy_row arr row_ix row =
            List.iteri (fun col_ix value ->
                    arr.(row_ix).(col_ix) <- {value = value; mark = false}) row in
        let (h, w) = RawBoard.size b in
        let arr = Array.make_matrix w h empty_cell in

        List.iteri (fun row_ix row -> copy_row arr row_ix row) b;
        arr


    let height = Array.length
    let width = Fun.flip Array.get 0 >> Array.length
    let size = Toolbox.fork width height


    let mark (board : board) (n : int) =
        let mark_number cell n =
            if cell.value == n then
                cell.mark <- true in

        Array.iter (Array.iter (Fun.flip mark_number n)) board


    let select_cells (board : board) =
        List.map (fun (x, y) -> board.(x).(y))


    let select_row (board : board) (row_ix : int) =
        select_cells board (List.init (fst (size board)) (fun x -> (x, row_ix)))


    let select_col (board : board) (col_ix : int) =
        select_cells board (List.init (snd (size board)) (fun y -> (col_ix, y)))


    let is_winning (board : board) =
        let select_by selector max =
            List.init max (selector board) in

        let is_winning_sequence =
            List.for_all (fun cell -> cell.mark) in

        List.exists is_winning_sequence (select_by select_row (width board))
            || List.exists is_winning_sequence (select_by select_col (height board))


    let sum_marked =
        let cell_map cell =
            if cell.mark then 0 else cell.value in
        let row_folder acc =
            Array.to_list >> List.map cell_map >> List.fold_left (+) acc in

        Array.fold_left (row_folder) 0


    let print (board : board) =
        let print_row row =
            printf "  [";
            Array.iter (fun cell -> printf "%2d%s " cell.value (if cell.mark then "•" else "◦")) row;
            printf "]\n"; in

        Toolbox.uncurry (printf "Board [%d × %d]:\n") (size board);
        Array.iter print_row board;
end


let read_header input =
    let parse_header =
        (String.split_on_char ',' >> List.filter_map Toolbox.Num.parse_int) in

    match input () with
        | Seq.Nil -> raise (Invalid_input "No header found")
        | Seq.Cons (line, rest) -> (parse_header line, rest)


let parse_input =
    let rec unfold_boards seq_node =
        match seq_node with
        | Seq.Nil -> []
        | stream -> (match RawBoard.of_seq (fun _ -> stream) with
            | (Some board, node) -> board :: (unfold_boards node)
            | (None, node)-> unfold_boards node
        ) in

    read_header >> Toolbox.second (fun s -> unfold_boards (s ()))


let convert_boards =
    List.filter RawBoard.validate
        >> List.filter (Fun.flip (>>) not RawBoard.empty)
        >> List.map Board.from_raw


let play_one (boards : Board.board list) (n : int) =
    List.iter (Fun.flip Board.mark n) boards;
    boards


let rec last_board (draws : int list) (boards : Board.board list) =
    let check_boards draw rest boards =
        match play_one boards draw with
        | [] -> raise (Invalid_input "No boards left")
        | [board] when Board.is_winning board -> (draw, board)
        | boards -> last_board rest (List.filter (Board.is_winning >> not) boards) in

    match draws with
        | [] -> raise (Invalid_input "Draws drained before board found")
        | draw::xd -> check_boards draw xd boards


let () =
    if Array.length Sys.argv - 1 <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 0;
    end;

    Toolbox.File.as_seq Sys.argv.(1)
        |> parse_input
        |> Toolbox.second convert_boards
        |> Toolbox.uncurry last_board
        |> fun (n, board) -> begin
            let board_value = Board.sum_marked board in
                printf "# drawn number = %d\n" n;
                printf "# sum = %d\n" board_value;
                printf "%d\n" (n * board_value);
        end
