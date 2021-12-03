module File = struct

    let as_seq file_name =
        let unfolder handle =
            try let line = input_line handle in
                Some (line, handle)
            with _ ->
                close_in_noerr handle;
                None
        and file = open_in file_name in
            Seq.unfold unfolder file

    let as_list file_name =
        as_seq file_name |> List.of_seq

end

module Num = struct

    let parse_int str =
        try Some (int_of_string str)
        with _ -> None

end

module Operators = struct

    let (|<) g f x = g (f x)
    let (>>) f g x = g (f x)

end


let both f (x, y) =
    (f x, f y)


let fork f g a =
    (f a, g a)
