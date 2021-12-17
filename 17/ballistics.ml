open Printf
open Toolbox
open Toolbox.Operators

exception Invalid_input of string


module Phys = struct
    module Range = struct
        type t = {
            min: int;
            max: int
        }

        let normalize ((a, b): int * int): (int * int) =
            if a > b then (b, a) else (a, b)

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

            In x direction (⟦a = -1⟧) the probe will travel for at most
            ⟦T = v₀⟧ steps, hence

            • we will undershoot if ⟦s(T) < x₁⟧ → ⟦v₀ < ½(3x₁ - 1)⟧,
            • we will overshoot if ⟦s(1) > x₂⟧ → ⟦v₀ > x₂⟧.
         *)

        let valid_vx (a: area): (int * int) =
            (3 * a.x.min - 1, a.x.max)

        (*  In y direction if ⟦v₀ > 0⟧, then the probe will
            travel upwards with ⟦a = -1⟧ until it reaches velocity ⟦v = 0⟧
            in ⟦T = v⟧ steps, reaching height

                                     v₀(v₀ + 1)
                H = s(T) = s(v₀) = a ----------
                                         2

            It will then fall down, reaching its initial position with speed
            ⟦v' = -(v + 1)⟧. From now on, we can shift the perspective
            as if the movement begins here, with ⟦a = 1⟧ and some ⟦v₀⟧.

            We cannot undershoot the area, as even with ⟦v₀ = 0⟧ the probe
            will accelerate downwards. We may overshoot it though if
            ⟦s(1) < y₂⟧ (y₂ is negative!) → ⟦v' < y₂⟧ → ⟦-v - 1 > y₂⟧ → ⟦v > 1 - y₂⟧.
        *)

        let valid_vy (a: area): (int * int) =
            (0, -1 - a.y.min)

        (*  Starting from position ⟦(0, 0)⟧ with speed ⟦v₀⟧ and acceleration
            ⟦a⟧, we want to know if there is any ⟦t⟧ such that

                A ≤ s(t) ∧ s(t) ≤ B

            for an area with boundary ⟦⟨A, B⟩⟧. We will find the smallest
            t, call it ⟦τ⟧, that satisfies the first inequality:

                D = (2v₀ - a)² + 8Aa

                    ┌╴                ╶┐
                    │  1               │
                τ = │ -- (√D - 2v + a) │
                    │ 2a               │

            Then we simply verify that ⟦s(τ) ≤ B⟧ to see whether
            we hit the area or not.
        *)

        module M = struct
            let pow n = n * n
        end

        let dist (v0: int) (a: int) (t: int): int =
            v0 * t + (a * t * (t - 1)) / 2

        let hits (bounds: int * int) (v0: int) (a: int): bool =
            let fv = Float.of_int v0 and fa = Float.of_int a in
            let d = Float.of_int (M.pow (2 * v0 - a) + 8 * a * (fst bounds)) in
            let ftau = (Float.sqrt(d) -. 2.0 *. fv +. fa) /. (2.0 *. fa) in
            let tau = Float.to_int (Float.ceil ftau) in

            assert (dist v0 a tau >= fst bounds);
            dist v0 a tau <= snd bounds
    end

    let max_height (a: area): int =
        let maximum =
            List.fold_left (fun a v -> if a > v then a else v) Int.min_int in

        (* Select valid v₀ for y speed *)
        Ballistics.valid_vy a |> (fun (a, b) -> a &-- (b + 1))
        (* Convert ⟦v⟧ upwards to ⟦v → -(v + 1)⟧ except that we will
           shift perspective, moving from ⟦(0, 0)⟧ to ⟦|yₘₐₓ|⟧, yielding
           ⟦v → v + 1⟧ *)
        |> List.map ((+) 1)
        (* Now we only want speeds that will hit the area (with the shifted
           perspective). *)
        |> List.filter (fun v -> Ballistics.hits (-a.y.max, -a.y.min) v 1)
        (* Shift the perspective back. *)
        |> List.map (Fun.flip (-) 1)
        (* Convert to height ⟦H(v) = s(v)⟧. *)
        |> List.map (fun v -> Ballistics.dist v (-1) v)
        (* And select the maximum. *)
        |> maximum
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

    File.as_seq Sys.argv.(1)
        |> parse_input
        |> Phys.max_height
        |> printf "%d\n"
