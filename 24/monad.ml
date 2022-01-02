open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Parser

module ALU = struct
    module Operand = struct
        type r = char
        let r_to_string = sprintf "‹%c›"

        type t =
            | Reg of r
            | Data of int

        let to_string = function
            | Reg p -> r_to_string p
            | Data n -> sprintf "%d" n
    end

    type op = Operand.t
    type reg = Operand.r

    module Instr = struct
        type t =
            | Inp of reg
            | Add of reg * op
            | Mul of reg * op
            | Div of reg * op
            | Mod of reg * op
            | Eql of reg * op

        let to_string =
            let strop = Operand.to_string
            and strr  = Operand.r_to_string in
            function
                | Inp v -> sprintf "inp %s" (strr v)
                | Add (a, b) -> sprintf "add %s %s" (strr a) (strop b)
                | Mul (a, b) -> sprintf "mul %s %s" (strr a) (strop b)
                | Div (a, b) -> sprintf "div %s %s" (strr a) (strop b)
                | Mod (a, b) -> sprintf "mod %s %s" (strr a) (strop b)
                | Eql (a, b) -> sprintf "eql %s %s" (strr a) (strop b)
    end

    type instr = Instr.t
    type prog = instr list

    module Registers = struct
        module ValueMap = Map.Make(Char)
        type t = int ValueMap.t

        let empty: t =
            ValueMap.empty

        let load (c: char) (reg: t): int =
            ValueMap.find_opt c reg |> Option.value ~default:0

        let store (c: char) (v: int) (reg: t): t =
            ValueMap.add c v reg

        let to_string: t -> string =
            ValueMap.bindings
                >> List.map (fun (c, v) -> sprintf "%c: %d" c v)
                >> String.concat ", "
    end

    type regs = Registers.t

    module Core = struct
        type state = regs * int list
        type state_r = (state, string) Result.t

        let init input: state_r =
            Ok (Registers.empty, input)

        let exec (i: instr): state_r -> state_r =
            let lvalue regs: reg -> int = Fun.flip Registers.load regs in
            let ovalue regs: op -> int = function
                | Reg r -> lvalue regs r
                | Data n -> n in

            let eval_m regs (f: int -> int -> int) (m: reg) (v: op) =
                let eval_f (op1: reg) (op2: op) =
                    f (lvalue regs op1) (ovalue regs op2) in
                Registers.store m (eval_f m v) regs in

            let exec' (regs, inp) =
                let lift reg' = Ok (reg', inp) in
                match i with
                | Inp r when List.length inp > 0 ->
                        Ok (Registers.store r (List.hd inp) regs, List.tl inp)
                | Inp _ -> Error "inp: Empty input"
                | Add (op1, op2) -> eval_m regs ( + ) op1 op2 |> lift
                | Mul (op1, op2) -> eval_m regs ( * ) op1 op2 |> lift
                | Div (_, op2) when ovalue regs op2 = 0 -> Error "div: Division by zero"
                | Div (op1, op2) -> eval_m regs ( / ) op1 op2 |> lift
                | Mod (op1, _) when lvalue regs op1 < 0 -> Error "mod: Numerator register value is negative"
                | Mod (_, op2) when ovalue regs op2 <= 0 -> Error "mod: Denominator is not positive"
                | Mod (op1, op2) -> eval_m regs (mod) op1 op2 |> lift
                | Eql (op1, op2) -> eval_m regs (fun a b -> if a = b then 1 else 0) op1 op2 |> lift in
            Fun.flip Result.bind exec'

        let exec_trace (i: instr): state_r -> state_r = function
            | Error e -> Error e
            | Ok (_, inp) as state -> begin
                printf "# • %s\t\t" (Instr.to_string i);
                List.map (sprintf "%d") inp |> String.concat " " |> printf "[%s]\n";
                let result = exec i state in begin
                match result with
                    | Ok (reg', _) -> printf "#   %s\n" (Registers.to_string reg')
                    | Error s -> printf "#   %s\n" s
                end;
                result
            end

        let run (inp: int list): prog -> state_r =
            List.fold_left (Fun.flip exec) (init inp)

        let trace (inp: int list): prog -> state_r =
            List.fold_left (Fun.flip exec_trace) (init inp)
    end
end

module SALU = struct
    module Instr = ALU.Instr
    type instr = Instr.t
    type prog = instr list

    module Operand = ALU.Operand
    type op = Operand.t
    type reg = Operand.r

    module Expr = struct
        type t =
            | Const of int
            | Input of int
            | Expr of int
            | Add of t * t
            | Sub of t * t
            | Mul of t * t
            | Div of t * t
            | Mod of t * t
            | Eql of t * t
            | Neq of t * t

        let eadd a b = Add (a, b)
        let emul a b = Mul (a, b)
        let ediv a b = Div (a, b)
        let emod a b = Mod (a, b)
        let eeql a b = Eql (a, b)

        let compare (e1: t) (e2: t) =
            let rank = function
                | Expr _ -> 5
                | Input _ -> 10
                | Const _ -> 20
                | Add _ -> 30
                | Sub _ -> 35
                | Mul _ -> 40
                | Div _ -> 45
                | Mod _ -> 47
                | Eql _ -> 50
                | Neq _ -> 55 in
            Int.compare (rank e1) (rank e2)

        let rec to_string = function
            | Const n -> sprintf "%d" n
            | Input k -> sprintf "‹%d›" k
            | Expr k -> sprintf "⟦%d⟧" k
            | Add (l, r) -> sprintf "(%s + %s)" (to_string l) (to_string r)
            | Sub (l, r) -> sprintf "(%s - %s)" (to_string l) (to_string r)
            | Mul (l, r) -> sprintf "(%s * %s)" (to_string l) (to_string r)
            | Div (l, r) -> sprintf "(%s / %s)" (to_string l) (to_string r)
            | Mod (l, r) -> sprintf "(%s %% %s)" (to_string l) (to_string r)
            | Eql (l, r) -> sprintf "(%s = %s)" (to_string l) (to_string r)
            | Neq (l, r) -> sprintf "(%s ≠ %s)" (to_string l) (to_string r)
    end

    type expr = Expr.t

    module Registers = struct
        module RegisterMap = Map.Make(Char)
        module ExprVersionMap = Map.Make(Int)

        type t = expr RegisterMap.t

        let empty: t =
            RegisterMap.empty

        let load (c: char) (reg: t): expr =
            RegisterMap.find_opt c reg |> Option.value ~default:(Expr.Const 0)

        let store (c: char) (v: expr) (reg: t): t =
            RegisterMap.add c v reg

        let to_string: t -> string list =
            RegisterMap.bindings
                >> List.map (fun (c, v) -> sprintf "%c: %s" c (Expr.to_string v))
    end

    type regs = Registers.t

    module Core = struct
        type state = regs * int
        type state_r = (state, string) Result.t

        let rec reduce e =
            let rec reorder: expr -> expr = function
                | Add (e1, e2) when Expr.compare e1 e2 > 0 -> Add (reorder e2, reorder e1)
                | Mul (e1, e2) when Expr.compare e1 e2 > 0 -> Mul (reorder e2, reorder e1)
                | Eql (e1, e2) when Expr.compare e1 e2 > 0 -> Eql (reorder e2, reorder e1)
                | Div (e1, e2) -> Div (reorder e1, reorder e2)
                | Mod (e1, e2) -> Mod (reorder e1, reorder e2)
                | e -> e in

            let reduce': expr -> expr = function
                | Add (Const a, Const b) -> Const (a + b)
                | Add (e, Const 0) | Add (Const 0, e) -> e
                | Add (Add (e, Const n), Const m) -> Add (e, Const (n + m))
                | Add (Const n, e) | Add (e, Const n) when n < 0 -> Sub (e, Const (-n))
                | Add (Mul (Const (-1), e1), e2) | Add (e2, Mul (Const (-1), e1)) -> Sub (e2, e1)
                | Sub (a, Sub (b, c)) -> Sub (Add (a, b), c)
                | Mul (Const a, Const b) -> Const (a * b)
                | Mul (e, Const 1) | Mul (Const 1, e) -> e
                | Mul (_, Const 0) | Mul (Const 0, _) -> Const 0
                | Mul (Mul (e, Const n), Const m) -> Mul (e, Const (n * m))
                | Div (Const n, Const k) -> Const (n / k)
                | Div (Const 0, _) -> Const 0
                | Div (e, Const 1) -> e
                | Div (e, Const n) when n < 0 -> Div (Mul (Const (-1), e), Const (-n))
                | Div (Div (e, Const n), Const k) -> Div (e, Const (n * k))
                | Mod (e, Const 1) -> e
                | Mod (Const 0, _) -> Const 0
                | Eql (e1, e2) when (reduce e1) = (reduce e2) -> Const 1
                | Eql (Const n, Const m) -> if n = m then Const 1 else Const 0
                | Eql (Const 0, Eql (e1, e2)) -> Neq (e1, e2)
                | e -> e in

            let e' = reduce' (reorder e) in
                if e' = e then e else reduce e'

        let init: state_r =
            Ok (Registers.empty, 0)

        let exec (opt: bool) (i: instr): state_r -> state_r =
            let lvalue regs: reg -> expr =
                Fun.flip Registers.load regs in
            let ovalue regs: op -> expr = function
                | Reg r -> lvalue regs r
                | Data n -> Expr.Const n in

            let eval_m regs (o: expr -> expr -> expr) (m: reg) (v: op) =
                let eval_f (op1: reg) (op2: op) =
                    o (lvalue regs op1) (ovalue regs op2) |> if opt then reduce else Fun.id in
                (Registers.store m (eval_f m v) regs) in
            let exec' (regs, inp) =
                let lift reg' = Ok (reg', inp) in
                match i with
                | Inp r -> Ok (Registers.store r (Expr.Input inp) regs, inp + 1)
                | Add (op1, op2) -> eval_m regs Expr.eadd op1 op2 |> lift
                | Mul (op1, op2) -> eval_m regs Expr.emul op1 op2 |> lift
                | Div (op1, op2) -> eval_m regs Expr.ediv op1 op2 |> lift
                | Mod (op1, op2) -> eval_m regs Expr.emod op1 op2 |> lift
                | Eql (op1, op2) -> eval_m regs Expr.eeql op1 op2 |> lift in

            Fun.flip Result.bind exec'

        let exec_trace (optimize: bool) (i: instr): state_r -> state_r = function
            | Error e -> Error e
            | Ok (_, _) as state -> begin
                printf "# • %s\n" (Instr.to_string i);
                let result = exec optimize i state in begin
                match result with
                    | Ok (reg', _) -> Registers.to_string reg' |> List.iter (printf "#    %s\n")
                    | Error s -> printf "#   %s\n" s
                end;
                result
            end

        let run' ?(trace = false) ?(optimize = false): prog -> state_r =
            let ef = Fun.flip ((if trace then exec_trace else exec) optimize) in
            List.fold_left ef init

        let run = run' ~trace:false
        let trace = run' ~trace:true
        let trace2 = run' ~trace:true ~optimize:true
    end
end

module MONAD = struct
    module Sequence = struct
        let next: int list -> int list =
            let rec decrement = function
                | [] -> []
                | [n] -> [n - 1]
                | k::rest -> match decrement rest with
                    | (-1)::xs -> (k - 1) :: 9 :: xs
                    | l -> k::l in

            decrement >> function
                | 0::_ -> []
                | l when Option.is_some (List.find_opt ((=) 0) l) -> decrement l
                | l -> l

        let create =
            let unfolder = function
                | [] -> None
                | l -> Some (l, next l) in
            Seq.unfold unfolder (List.replicate 14 9)

        let to_int =
            let rec to_int' a = function
                | [] -> a
                | n::rest -> to_int' (a * 10 + n) rest in
            to_int' 0

        let to_string =
            List.map (sprintf "%d") >> String.concat " "

        let to_number_string =
            List.map (sprintf "%d") >> String.concat ""

        let to_string_opt = function
            | None -> "⊥"
            | Some l -> to_string l
    end

    class generator = object (self)
        val m = Mutex.create ()
        val mutable s = Sequence.create
        val mutable c = 0

        method private uc n =
            c <- c + n;
            printf "# %d\x1b[0K\r%!" c

        method private next' = match s () with
            | Nil -> None
            | Cons (l, s') ->
                    self#uc 1;
                    s <- s';
                    Some l

        method next =
            Mutex.lock m;
            let result = self#next' in
                Mutex.unlock m;
                result

        method private next_batch' n = let (result, s') = Seq.copy_rev n s in
            s <- s';
            self#uc n;
            result

        method next_batch n =
            Mutex.lock m;
            let result = self#next_batch' n in
                Mutex.unlock m;
                result

        method stop' =
            s <- Seq.empty

        method stop =
            Mutex.lock m;
            self#stop';
            Mutex.unlock m
    end

    class collector = object
        val m = Mutex.create ()
        val mutable r: int list list = []

        method get =
            Mutex.lock m;
            let result = List.rev r in
                Mutex.unlock m;
                result

        method add l =
            Mutex.lock m;
            r <- l::r;
            Mutex.unlock m
    end

    let threads_count = 5

    let run mp =
        let worker (g, c) =
            let rec worker' g = function
                | [] -> begin match g#next_batch 4096 with
                    | [] -> ()
                    | l -> worker' g l
                end
                | inp::rest ->  match ALU.Core.run inp mp with
                    | Ok (r, _) when ALU.Registers.load 'z' r = 0 ->
                        g#stop;
                        c#add inp;
                        ()
                    | _ -> worker' g rest in
            worker' g [] in

        let g = new generator
        and c = new collector in

        (0 &-- threads_count)
            |> List.map (fun _ -> Thread.create worker (g, c))
            |> List.iter (fun thread -> Thread.join thread; printf "# A worker has finished\n");

        printf "# Collected results:\n";
        c#get
            |> List.map Sequence.to_int
            |> List.sort Int.compare
            |> List.iter (printf "%d\n")
end

module Program = struct
    module Parser = struct
        include CharParser

        let register = any_of "xyzw"
        let operand =
                (register |> map (fun c -> ALU.Operand.Reg c))
            <|> (integer |> map (fun i -> ALU.Operand.Data i))

        let unwrap1 (r, l) = match l with
            | [op] -> (r, op)
            | _ -> raise (Failure "unwrap1: Invalid number of operands")

        let prinstr name operands =
            str name @>> combine (symbol ' ' @>> register) (count operands (symbol ' ' @>> operand))

        let pinp = prinstr "inp" 0 |> map (fun (r, _) -> ALU.Instr.Inp r)
        let padd = prinstr "add" 1 |> map unwrap1 |> map (fun (a, b) -> ALU.Instr.Add (a, b))
        let pmul = prinstr "mul" 1 |> map unwrap1 |> map (fun (a, b) -> ALU.Instr.Mul (a, b))
        let pdiv = prinstr "div" 1 |> map unwrap1 |> map (fun (a, b) -> ALU.Instr.Div (a, b))
        let pmod = prinstr "mod" 1 |> map unwrap1 |> map (fun (a, b) -> ALU.Instr.Mod (a, b))
        let peql = prinstr "eql" 1 |> map unwrap1 |> map (fun (a, b) -> ALU.Instr.Eql (a, b))

        let instr =
            choose [pinp; padd; pmul; pdiv; pmod; peql]

        let parse =
            Seq.resident >> run_exn (sep_by eol instr) ()
    end
end

module Main = struct
    let usage _ =
        Printf.eprintf "usage: %s exe FILE [N…]\n" Sys.argv.(0);
        Printf.eprintf "       %s trace FILE [N…]\n" Sys.argv.(0);
        Printf.eprintf "       %s sym-exe FILE\n" Sys.argv.(0);
        Printf.eprintf "       %s sym-exe-opt FILE\n" Sys.argv.(0);
        Printf.eprintf "       %s sym-trace FILE\n" Sys.argv.(0);
        Printf.eprintf "       %s sym-trace-opt FILE\n" Sys.argv.(0);
        Printf.eprintf "       %s monad-bf FILE\n" Sys.argv.(0);
        exit 1

    let argc = Array.length Sys.argv

    let run_exe trace file_name inp_arg =
        let input = (inp_arg &-- argc)
            |> List.map (fun i -> Num.parse_int_exn Sys.argv.(i)) in

        File.as_char_seq file_name
            |> Program.Parser.parse
            |> (if trace then ALU.Core.trace else ALU.Core.run) input
            |> function
                | Ok (reg, _) -> printf "# result %s\n" (ALU.Registers.to_string reg)
                | Error s -> printf "# %s\n" s

    let run_symb_exe trace optimize file_name =
        File.as_char_seq file_name
            |> Program.Parser.parse
            |> SALU.Core.run' ~trace:trace ~optimize:optimize
            |> function
                | Ok (reg, n) -> printf "# Result:\n";
                    SALU.Registers.to_string reg |> List.iter (printf "#    %s\n");
                    printf "# Used %d input variables\n" n
                | Error s -> printf "# %s\n" s

    let run_monad file_name =
        File.as_char_seq file_name
            |> Program.Parser.parse
            |> MONAD.run

    let check_args n =
        if argc < n then usage ()

    let run =
        Printexc.record_backtrace true;

        check_args 2;

        match Sys.argv.(1) with
            | "exe" -> check_args 3; run_exe false Sys.argv.(2) 3
            | "trace" -> check_args 3; run_exe true Sys.argv.(2) 3
            | "sym-exe" -> check_args 3; run_symb_exe false false Sys.argv.(2)
            | "sym-exe-opt" -> check_args 3; run_symb_exe false true Sys.argv.(2)
            | "sym-trace" -> check_args 3; run_symb_exe true false Sys.argv.(2)
            | "sym-trace-opt" -> check_args 3; run_symb_exe true true Sys.argv.(2)
            | "monad-bf" -> check_args 3; run_monad Sys.argv.(2)
            | _ -> check_args 2; run_exe false Sys.argv.(1) 2
end

let () =
    Main.run
