(* Sudoku solver using Z3 *)

open Z3
open Z3.Arithmetic.Integer
open Z3.Boolean
open Z3.Solver
open Z3.Arithmetic
open Z3.Model

let solve_sudoku grid =
  let ctx = mk_context [] in
  let solver = mk_solver ctx None in
  let cells = Array.make_matrix 9 9 (mk_numeral_s ctx "x") in

  for i = 0 to 8 do
    for j = 0 to 8 do
      let cell = cells.(i).(j) in
      Solver.add solver
        [
          mk_le ctx (mk_numeral_i ctx 1) cell;
          mk_le ctx cell (mk_numeral_i ctx 9);
        ]
    done
  done;

  for i = 0 to 8 do
    let row = Array.to_list (Array.init 9 (fun j -> cells.(i).(j))) in
    Solver.add solver [ mk_distinct ctx row ]
  done;

  for j = 0 to 8 do
    let col = Array.to_list (Array.init 9 (fun i -> cells.(i).(j))) in
    Solver.add solver [ mk_distinct ctx col ]
  done;

  for i = 0 to 2 do
    for j = 0 to 2 do
      let square =
        Array.to_list
          (Array.init 9 (fun k ->
               cells.((3 * i) + (k / 3)).((3 * j) + (k mod 3))))
      in
      Solver.add solver [ mk_distinct ctx square ]
    done
  done;

  for i = 0 to 8 do
    for j = 0 to 8 do
      match grid.(i).(j) with
      | None -> ()
      | Some n ->
          Solver.add solver [ mk_eq ctx cells.(i).(j) (mk_numeral_i ctx n) ]
    done
  done;

  match Solver.check solver [] with
  | SATISFIABLE ->
      let model = get_model solver in
      let model = Option.get model in
      let solution =
        Array.init 9 (fun i ->
            Array.init 9 (fun j ->
                match get_const_interp_e model cells.(i).(j) with
                | Some n -> numeral_to_string n
                | None -> "-"))
      in
      solution
  | _ -> failwith "Unsolvable"

let read_grid_from_file file_name =
  let ic = open_in file_name in
  let grid = Array.make_matrix 9 9 None in
  try
    for i = 0 to 8 do
      let line = input_line ic in
      let values = String.split_on_char ' ' line in
      List.iteri
        (fun j v ->
          grid.(i).(j) <- (if v = "_" then None else Some (int_of_string v)))
        values
    done;
    close_in ic;
    grid
  with _ ->
    close_in ic;
    failwith "Invalid grid"

let print_solution solution =
  Array.iter
    (fun row ->
      Array.iter print_string row;
      print_newline ())
    solution

let () =
  print_endline "Sudoku solver using Z3";
  print_endline "Enter the file name containing the grid:";
  let file_name = read_line () in
  let grid = read_grid_from_file file_name in
  let solution = solve_sudoku grid in
  print_endline "Solution:";
  print_solution solution
