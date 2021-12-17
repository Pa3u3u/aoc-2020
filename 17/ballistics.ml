open Printf
open Toolbox
open Toolbox.Operators
open Toolbox.Pair

exception Invalid_input of string


module Phys = struct
    module Range = struct
        type t = {
            min: int;
            max: int
        }

        let normalize ((a, b): int * int): (int * int) =
            if Int.abs a > Int.abs b then (b, a) else (a, b)

        let from_tuple: (int * int) -> t =
            normalize >> fun (min, max) -> { min; max }

        let to_string (r: t): string =
            sprintf "[%d … %d]" r.min r.max
    end

    type r = Range.t


    module Area = struct
        type t = {
            x: r;
            y: r
        }

        let from_tuples xr yr: t =
            { x = Range.from_tuple xr; y = Range.from_tuple yr }

        let to_string (a: t): string =
            sprintf "area x:%s y:%s" (Range.to_string a.x) (Range.to_string a.y)
    end

    type area = Area.t


    module Ballistics = struct
        (*  The length of a trajectory of an object that moves with an
            initial speed ⟦v₀⟧ and accelecation ⟦a⟧ is generally

                s(t) = ∫[0,t] v(t) dt

            in discrete terms of this task

                s(t) = Σ[1,t] v(t)

            where ⟦v(t) = v₀ + at⟧, so we get

                s(t) = v₀t + ½at(t - 1)

        *)

        let dist (v0: int) (a: int) (t: int): int =
            v0 * t + (a * t * (t - 1)) / 2

        (*
            In x direction (⟦a = -1⟧) the probe will travel for at most
            ⟦T = v₀⟧ steps, hence

            • we will overshoot if ⟦s(1) > x₂⟧ → ⟦v₀ > x₂⟧.
            • we will undershoot if ⟦s(T) < x₁⟧ for ⟦T = v⟧, which yields
              the minimum initial speed
                         ______________
                     a + √a² + 8x₁(a + 2)
                v₀ ≥ --------------------
                           2(2 + a)

              and for ⟦a = -1⟧ we get simply

                     1        _______
                v₀ < - (-1 + √1 + 8x₁)
                     2
         *)

        let valid_vx (a: area): (int * int) =
            let d = 1.0 +. 8.0 *. (Float.of_int a.x.min) in
            let minv = Float.ceil((0.5) *. (Float.sqrt d -. 1.0)) |> Float.to_int in

            (minv, a.x.max)

        (*  In y direction, we will shift the directive. If ⟦v₀ > 0⟧, then
            the probe will travel upwards with ⟦a = -1⟧ until it reaches
            velocity ⟦v = 0⟧ in ⟦T = v₀⟧ steps, reaching height

                                   1
                H = s(T) = s(v₀) = - v₀ (v₀ + 1)
                                   2

            It will then fall down, reaching its initial position with speed
            ⟦v' = -(v + 1)⟧ at time ⟦2 * |v₀| + 1⟧.

            We cannot undershoot the area, as even with ⟦v₀ = 0⟧ the probe
            will accelerate downwards. We may overshoot it though in two
            cases:
              • we shoot downwards with such an initial speed that
                ⟦s(1) < y₂⟧, that is, ⟦v₀ < y₂⟧,
              • we shoot upwards with such a speed that when the probe
                gets back to ⟦(0, 0)⟧, its speed ⟦v'⟧ will overshoot,
                that is, ⟦s'(1) < y₂⟧ → ⟦v₀' < y₂⟧ → ⟦-(v₀ + 1) > y₂⟧
                → ⟦v₀ > -(1 + y₂)⟧.
        *)

        let valid_vy (a: area): (int * int) =
            (a.y.max, -(1 + a.y.max))

        (*  For part one we only consider ⟦v₀ < 0⟧, that is, ⟦v₀' > 0⟧ when
            the probe gets back to ⟦(0, 0)⟧. Here, with acceleration
            ⟦a = -1⟧, we want to know if there is any ⟦t⟧ such that

                A ≤ s'(t) ∧ s'(t) ≤ B

            for an area with boundary ⟦⟨A, B⟩⟧. We will find the smallest
            ⟦t⟧, call it ⟦τ⟧, that satisfies the first inequality:

                D = (2v₀ - a)² + 8Aa

                    ┌╴                ╶┐
                    │  1            _  │
                τ = │ -- (a - 2v ± √D) │
                    │ 2a               │

            From the two possibilities we of course only choose ⟦t ≥ 0⟧.
            Then we simply verify that ⟦s'(τ) ≤ B⟧ to see whether
            we hit the area or not.
        *)

        module M = struct
            let pow n = n * n
        end

        let tau (bounds: int * int) (v0: int) (a: int): int =
            let fv = Float.of_int v0 and fa = Float.of_int a in
            let d = Float.of_int (M.pow (2 * v0 - a) + 8 * a * (fst bounds)) in
            let mul = fa -. 2.0 *. fv and divisor = 2.0 *. fa in
            let ftau1 = (mul +. Float.sqrt(d)) /. divisor in
            let ftau2 = (mul -. Float.sqrt(d)) /. divisor in
            let ftau' = if ftau1 >= 0.0 then ftau1 else ftau2 in
            let tau' = Float.to_int (Float.ceil ftau') in

            assert (tau' >= 0);
            assert (Int.abs (dist v0 a tau') >= Int.abs(fst bounds));
            tau'

        let hits (bounds: int * int) (v0: int) (a: int): bool =
            let tau = tau bounds v0 a in
            Int.abs(dist v0 a tau) <= Int.abs(snd bounds)

        let times_x (bounds: int * int) (v0: int): int Seq.t =
            let a = -1 in
            let tau = tau bounds v0 a in
            Seq.populate (fun t -> dist v0 a t <= snd bounds) ((+) 1) tau

        let times_y (bounds: int * int) (v0: int): int Seq.t =
            let a = -1 in
            (* If ⟦v₀ > 0⟧, we need to add the time it takes for the probe
               to return back to point ⟦(0, 0)⟧. *)
            let (v0', dt) = if v0 > 0 then (-(v0 + 1), 2 * v0 + 1)
                                      else (v0, 0) in
            let tau = tau bounds v0' a in
            Seq.populate (fun t -> Int.abs (dist v0' a (t - dt)) <= Int.abs (snd bounds)) ((+) 1) (tau + dt)
    end

    let max_height (a: area): int =
        let maximum =
            List.fold_left (fun a v -> if a > v then a else v) Int.min_int in

        (* Select valid (upwards) v₀ for y speed *)
        Ballistics.valid_vy a
            |> (fun (a, b) -> a &-- (b + 1))
            |> List.filter (fun v -> v > 0)
        (* ⟦v₀ > 0⟧ will become ⟦v₀' = -(v₀ + 1)⟧ when it gets back. *)
        (* Now we only want speeds that will hit the area. *)
        |> List.filter (fun v -> Ballistics.hits (a.y.min, a.y.max) (-(v + 1)) (-1))
        (* Convert to height ⟦H(v) = s(v)⟧. *)
        |> List.map (fun v -> Ballistics.dist v (-1) (Int.abs v))
        (* And select the maximum. *)
        |> maximum

    let count_positions (a: area): int =
        let rec has_common (sa: int Seq.t) (sb: int Seq.t) = match sa (), sb () with
            | Nil, _ -> false
            | _, Nil -> false
            | Cons (a, xa), Cons (b,  _) when a < b -> has_common xa sb
            | Cons (a,  _), Cons (b, xb) when a > b -> has_common sa xb
            | Cons (_,  _), Cons (_,  _) -> true in

        let hits_target (a: area) vx vy =
            (* For given speeds ⟦vx⟧ and ⟦vy⟧ we compute sets
               ⟦Tx = {t | A ≤ s(t) ≤ B}⟧ (probe is in the area) and similarly
               ⟦Ty⟧. *)
            let times_x = Ballistics.times_x (a.x.min, a.x.max) vx
            and times_y = Ballistics.times_y (a.y.min, a.y.max) vy in
            (* Then ⟦vx, vy⟧ is a valid initial vector, if ⟦Tx ∩ Ty ≠ ø⟧. *)
            has_common times_x times_y in

        let x_positions = Ballistics.valid_vx a |> fun (a, b) -> (a &-- (b + 1)) in
        let y_positions = Ballistics.valid_vy a |> fun (a, b) -> (a &-- (b + 1)) in

        List.product x_positions y_positions
            |> List.filter (uncurry (hits_target a))
            |> List.length
end


let parse_input: string Seq.t -> Phys.area =
    let regex = Str.regexp "target area: x=\\(-?[0-9]+\\)..\\(-?[0-9]+\\), y=\\(-?[0-9]+\\)..\\(-?[0-9]+\\)" in
    let parse_line line =
        if not (Str.string_match regex line 0) then
            raise (Invalid_input (sprintf "Syntax error in input: '%s'" line));
        let group = Fun.flip Str.matched_group line in
        match List.init 4 ((+) 1 >> group) |> List.map Num.parse_int_exn with
            | [x1;x2;y1;y2] -> Phys.Area.from_tuples (x1, x2) (y1, y2)
            | _ -> raise (Failure "Invalid input") in
   List.of_seq >> List.map parse_line >> List.hd


let () =
    let argc = Array.length Sys.argv - 1 in

    if argc <> 1 then begin
        Printf.eprintf "usage: %s FILE\n" Sys.argv.(0);
        exit 1;
    end;

    Printexc.record_backtrace true;
    File.as_seq Sys.argv.(1)
        |> parse_input
        |> Phys.count_positions
        |> printf "%d\n"
