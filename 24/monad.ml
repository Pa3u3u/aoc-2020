open Printf
open Toolbox.Core
open Toolbox.Extensions
open Toolbox.Operators
open Toolbox.Pair
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

    module Options = struct
        type t = {
            analyze: bool;
            constraints: bool;
            optimize: bool;
            trace: bool;
        }

        let default: t = {
            analyze = false;
            constraints = false;
            optimize = false;
            trace = false
        }
    end

    type options = Options.t

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

        let run (options: options) (inp: int list): prog -> state_r =
            let fold = Fun.flip (if options.trace then exec_trace else exec) in
            List.fold_left fold (init inp)
    end
end

module SALU = struct
    module Instr = ALU.Instr
    type instr = Instr.t
    type prog = instr list

    module Operand = ALU.Operand
    type op = Operand.t
    type reg = Operand.r

    module Operation = struct
        type t = Add | Mul | Div | Mod | Eql | Neq

        let is_commutative = function
            | Add | Mul | Eql | Neq -> true
            | _ -> false

        let rank = function
            | Add -> 30
            | Mul -> 40
            | Div -> 45
            | Mod -> 47
            | Eql -> 50
            | Neq -> 55

        let to_string = function
            | Add -> "+"
            | Mul -> "*"
            | Div -> "÷"
            | Mod -> "%"
            | Eql -> "="
            | Neq -> "≠"
    end

    module Expr = struct
        type t =
            | Const of int
            | Input of int
            | Expr of int
            | Op2 of Operation.t * t * t

        let eadd a b = Op2 (Operation.Add, a, b)
        let emul a b = Op2 (Operation.Mul, a, b)
        let ediv a b = Op2 (Operation.Div, a, b)
        let emod a b = Op2 (Operation.Mod, a, b)
        let eeql a b = Op2 (Operation.Eql, a, b)

        let compare (e1: t) (e2: t) =
            let rank = function
                | Expr _ -> 5
                | Input _ -> 10
                | Const _ -> 20
                | Op2 (f, _, _) -> Operation.rank f in

            Int.compare (rank e1) (rank e2)

        let rec to_string = function
            | Const n -> sprintf "%d" n
            | Input k -> sprintf "‹%d›" k
            | Expr k -> sprintf "⟦%d⟧" k
            | Op2 (f, l, r) -> sprintf "(%s %s %s)" (to_string l) (Operation.to_string f) (to_string r)
    end

    type expr = Expr.t

    module Registers = struct
        module CharMap = Map.Make(Char)
        module IntMap = Map.Make(Int)

        type t = (int CharMap.t * (char * expr) IntMap.t)

        let empty: t =
            (CharMap.empty, IntMap.empty)

        let load (c: char) (reg: t): expr =
            CharMap.find_opt c (fst reg)
                |> Option.map (fun n -> Expr.Expr n)
                |> Option.value ~default:(Expr.Const 0)

        let store (c: char) (v: expr) ((im, em): t): t =
            let index = IntMap.cardinal em in
                (CharMap.add c index im, IntMap.add index (c, v) em)

        let to_string: t -> string list =
            snd >> IntMap.bindings
                >> List.map (fun (i, (c, e)) -> sprintf "%4d %c <- %s" i c (Expr.to_string e))
    end

    type regs = Registers.t
    type options = ALU.options

    module Core = struct
        type state = regs * int
        type state_r = (state, string) Result.t

        let rec reduce e =
            let rec reorder: expr -> expr = function
                | Op2 (f, e1, e2) when Operation.is_commutative f && Expr.compare e1 e2 > 0 ->
                        Op2 (f, reorder e2, reorder e1)
                | Op2 (Div, e1, e2) -> Op2 (Div, reorder e1, reorder e2)
                | Op2 (Mod, e1, e2) -> Op2 (Mod, reorder e1, reorder e2)
                | e -> e in

            let reduce': expr -> expr = function
                | Op2 (Add, Const a, Const b) -> Const (a + b)
                | Op2 (Add, e, Const 0) | Op2 (Add, Const 0, e) -> e
                | Op2 (Add, Op2 (Add, e, Const n), Const m) -> Op2 (Add, e, Const (n + m))

                | Op2 (Mul, Const a, Const b) -> Const (a * b)
                | Op2 (Mul, e, Const 1) | Op2 (Mul, Const 1, e) -> e
                | Op2 (Mul, _, Const 0) | Op2 (Mul, Const 0, _) -> Const 0
                | Op2 (Mul, Op2 (Mul, e, Const n), Const m) -> Op2 (Mul, e, Const (n * m))

                | Op2 (Div, Const n, Const k) -> Const (n / k)
                | Op2 (Div, e, Const 1) -> e
                | Op2 (Div, e, Const n) when n < 0 -> Op2 (Div, Op2 (Mul, Const (-1), e), Const (-n))
                | Op2 (Div, Op2 (Div, e, Const n), Const k) -> Op2 (Div, e, Const (n * k))

                | Op2 (Mod, e, Const 1) -> e
                | Op2 (Mod, Const 0, _) -> Const 0

                | Op2 (Eql, e1, e2) when (reduce e1) = (reduce e2) -> Const 1
                | Op2 (Eql, Const n, Const m) -> Const (if n = m then 1 else 0)
                | Op2 (Eql, Const 0, Op2 (Eql, e1, e2)) -> Op2 (Neq, e1, e2)
                | e -> e in

            let e' = reduce' (reorder e) in
            if e' = e then e else reduce e'

        module RIM = Registers.IntMap
        module IntSet = Set.Make(Int)

        let optimize2: state_r -> state_r =
            let initial =
                Registers.CharMap.bindings >> List.map snd >> IntSet.of_list in
            let refs =
                let rec refs' s = function
                    | Expr.Expr k -> IntSet.add k s
                    | Op2 (_, e1, e2)  -> refs' (refs' s e1) e2
                    | _ -> s in
                refs' IntSet.empty in
            let check_reach used (index, (_, expr)) =
                if not (IntSet.mem index used) then
                    used
                else
                    IntSet.union used (refs expr) in
            let purge em used =
                RIM.filter (fun k _ -> IntSet.mem k used) em in
            let remove_unused (im, em) =
                List.fold_left check_reach (initial im) (RIM.bindings em |> List.rev)
                    |> purge em |> fun em' -> (im, em') in

            let rebuild_refs (im, em) =
                let is_used s expr =
                    RIM.exists (fun _ (_, e) -> expr = e) s in
                let get_ref s expr =
                    RIM.bindings s |> List.find (fun (_, (_, e)) -> expr = e) |> fst in
                let rec rebuild' s = function
                        | Expr.Input k -> Expr.Input k
                        | e when is_used s e -> Expr.Expr (get_ref s e)
                        | Expr.Op2 (f, e1, e2) -> Op2 (f, rebuild' s e1, rebuild' s e2)
                        | e -> e in
                let rebuild (s: (char * expr) RIM.t) ((index, (r, expr)): int * (char * expr)) =
                    RIM.add index (r, rebuild' s expr) s in

                RIM.bindings em |> List.fold_left rebuild RIM.empty |> fun em' -> (im, em') in

            let is_simple = function
                | Expr.Const _ -> true
                | Expr.Input _ -> true
                | Expr.Op2 (f, e1, e2) when f != Eql && f != Neq ->
                        (IntSet.cardinal (refs e1)) <= 1
                    &&  (IntSet.cardinal (refs e2)) <= 1
                | _ -> false in

            let rec analyze em (index, (r, expr)) =
                let get_expr k = RIM.find k em |> snd
                and get_reg k = RIM.find k em |> fst in

                begin match expr with
                    | Expr.Expr k ->
                            RIM.find k em |> snd
                    | Op2 (f, Expr k, e2) when get_reg k = r || is_simple (get_expr k) ->
                        Op2 (f, get_expr k, e2)
                    | e -> e
                end |> reduce |> fun e ->
                    if e = expr
                        then RIM.add index (r, e) em
                        else analyze em (index, (r, e)) in

            let optimize2' em =
                RIM.bindings em |> List.fold_left analyze em in

            Result.map (first (second optimize2' >> remove_unused >> rebuild_refs))

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

        let run (options: options): prog -> state_r =
            let ef = Fun.flip ((if options.trace then exec_trace else exec) options.optimize) in
            List.fold_left ef init
                >> (if options.analyze then optimize2 else Fun.id)
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

    let threads_count = 256

    let brute_force mp =
        let worker (g, c) =
            let rec worker' g = function
                | [] -> begin match g#next_batch 4096 with
                    | [] -> ()
                    | l -> worker' g l
                end
                | inp::rest ->  match ALU.Core.run ALU.Options.default inp mp with
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

    let print_analysis1 (em, _) =
        printf "# -- SALU state\n";
        SALU.Registers.to_string em |> List.iter (printf "#    %s\n")

    module MonadALU = struct
        module Expr = struct
            type t =
                | Neq of int * int
                | Peek
                | Pop
                | PushAdd of int * int

            let to_string = function
                | Peek -> "top <- peek"
                | Pop -> "top <- pop"
                | Neq (n, k) -> sprintf "if   ‹%d› ≠ ⟦top⟧ %+d" n k
                | PushAdd (n, k) -> sprintf "then push ‹%d› %+d" n k
        end

        type expr = Expr.t

        module AIN = ALU.Instr
        let transform: ALU.instr list -> expr list =
            let rec traverse n: ALU.instr list -> expr list = function
                | [] -> []
                | (AIN.Inp 'w')::rest
                    -> traverse (n + 1) rest
                | (AIN.Mul ('x', Data 0))::(AIN.Add ('x', Reg 'z'))::(AIN.Mod ('x', Data 26))::(AIN.Div ('z', Data k))::rest
                    -> (if k = 1 then Peek else Pop) :: traverse n rest
                | (AIN.Add ('x', Data k))::(AIN.Eql ('x', Reg 'w'))::(AIN.Eql ('x', Data 0))::rest
                    -> Neq (n, k) :: traverse n rest
                | (AIN.Mul ('y', Data 0))::(AIN.Add ('y', Data 25))::(AIN.Mul ('y', Reg 'x'))
                    ::(AIN.Add ('y', Data 1))::(AIN.Mul ('z', Reg 'y'))::rest
                    -> traverse n rest
                | (AIN.Mul ('y', Data 0))::(AIN.Add ('y', Reg 'w'))::(AIN.Add ('y', Data k))
                    ::(AIN.Mul ('y', Reg 'x'))::(AIN.Add ('z', Reg 'y'))::rest
                    -> PushAdd (n, k) :: traverse n rest
                | rest -> begin
                    eprintf "# MONAD: Pattern matching failed:\n";
                    rest |> List.take 8 |> List.iter (AIN.to_string >> printf "#   %s\n");
                    raise (Failure "Invalid MONAD program")
                end
            in
            traverse (-1)
    end

    module Constraint = struct
        type t =
            | Eq of int * int * int (* ‹a› = ‹b› + c *)
            | Ne of int * int * int

        let to_string = function
            | Eq (p, q, d) -> sprintf "‹%d› = (‹%d›%+d) %% 26" p q d
            | Ne (p, q, d) -> sprintf "‹%d› ≠ (‹%d›%+d) %% 26" p q d
    end

    type con = Constraint.t

    let build_constraints =
        let top = List.hd and pop = List.tl in

        let rec traverse c s: MonadALU.expr list -> con list = function
            | [] -> c
            | Peek :: Neq (n, k) :: PushAdd (n', l) :: rest
                ->  assert (n = n');
                    begin match s with
                        | [] -> traverse c ((n, l)::s) rest
                        | (m, p)::_ -> traverse (Ne (n, m, p + k)::c) ((n, l)::s) rest
                    end
            | Pop :: Neq (n, k) :: PushAdd (n', _) :: rest
                ->  assert (n = n');
                    let (m, p) = top s in
                    traverse ((Eq (n, m, k + p))::c) (pop s) rest
            | _ -> raise (Failure "Invalid MONAD expression list") in
        traverse [] [] >> List.rev

    let analyze mp =
        MonadALU.transform mp
            |> build_constraints
            |> List.map (function
                | Constraint.Eq (p, q, n) -> (p, Constraint.Eq (p, q, n))
                | Ne (p, q, n) -> (p, Ne (p, q, n)))

    module ConstrainedSequence = struct
        module IntMap = Map.Make(Int)
        type m = int IntMap.t

        let rebuild m =
            (0 &-- 14) |> List.map (fun k -> IntMap.find k m)

        let generate cb cstr =
            let filter m (c: Constraint.t option) k = match c with
                | None -> true
                | Some (Eq (_, p, v)) -> (IntMap.find p m) + v = k
                | Some (Ne (_, p, v)) -> (IntMap.find p m) + v != k in
            let rec choose n m cstr =
                let c = List.assoc_opt n cstr in
                if n = 14
                then cb (rebuild m)
                else (1 &-- 10) |> List.rev
                    |> List.filter (fun k -> filter m c k)
                    |> List.fold_left (fun s k -> s || choose (n + 1) (IntMap.add n k m) cstr) false in
            choose 0 IntMap.empty cstr
    end

    let run (options: ALU.Options.t) mp =
        let run_simple input =
            ALU.Core.run ALU.Options.default input mp |> function
                | Ok (em, _) -> if ALU.Registers.load 'z' em = 0
                    then begin
                        printf "%s\n" (Sequence.to_number_string input);
                        true
                    end else
                        false
                | Error _ -> false in

        if options.constraints then
            analyze mp
                |> ConstrainedSequence.generate run_simple
                |> ignore
        else
            brute_force mp
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
        Printf.eprintf "usage: %s exe (-r|-t) FILE [N…]\n" Sys.argv.(0);
        Printf.eprintf "       %s sym (-r|-aot) FILE\n" Sys.argv.(0);
        Printf.eprintf "       %s monad (-r|-c) FILE\n" Sys.argv.(0);
        exit 1

    let argc = Array.length Sys.argv

    let get_options str: ALU.Options.t =
        if String.length str = 0 || str.[0] != '-' then begin
            eprintf "sym: Invalid options '%s'\n" str;
            usage ()
        end;

        let options = ['r'; 'a'; 'c'; 'o'; 't'] in begin
            if (String.to_seq str |> Seq.filter ((!=) '-')
                    |> Seq.filter (fun c -> List.find_opt ((=) c) options  |> Option.is_none)
                    |> List.of_seq |> List.length) != 0 then begin
                eprintf "sym: Unknown options '%s', expected '-r' or '-[a|o|c|t]'\n" str;
                usage ()
            end;

            {
                analyze = String.contains str 'a';
                constraints = String.contains str 'c';
                trace = String.contains str 't';
                optimize = String.contains str 'o'
            }
        end

    let run_exe options file_name inp_arg =
        let input = (inp_arg &-- argc)
            |> List.map (fun i -> Num.parse_int_exn Sys.argv.(i)) in

        File.as_char_seq file_name
            |> Program.Parser.parse
            |> ALU.Core.run options input
            |> function
                | Ok (reg, _) -> printf "# Result %s\n" (ALU.Registers.to_string reg)
                | Error s -> printf "# %s\n" s

    let run_symb options file_name =
        File.as_char_seq file_name
            |> Program.Parser.parse
            |> SALU.Core.run options
            |> function
                | Ok (reg, n) -> printf "# Result:\n";
                    SALU.Registers.to_string reg |> List.iter (printf "#    %s\n");
                    printf "# Used %d input variables\n" n
                | Error s -> printf "# %s\n" s

    let run_monad options file_name =
        File.as_char_seq file_name
            |> Program.Parser.parse
            |> MONAD.run options

    let check_args n =
        if argc < n then usage ()

    let run =
        Printexc.record_backtrace true;

        check_args 2;

        match Sys.argv.(1) with
            | "exe" -> check_args 4; run_exe (get_options Sys.argv.(2)) Sys.argv.(3) 4
            | "sym" -> check_args 4; run_symb (get_options Sys.argv.(2)) Sys.argv.(3)
            | "monad" -> check_args 4; run_monad (get_options Sys.argv.(2)) Sys.argv.(3)
            | _ -> check_args 2; run_exe ALU.Options.default Sys.argv.(1) 2
end

let () =
    Main.run
