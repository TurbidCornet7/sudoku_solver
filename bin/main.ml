open Z3
(**
    Sudoku solver using z3
    *)

let int_of_expr e = Z.to_int (Arithmetic.Integer.get_big_int e)

(*abifunktsioon laua lugemiseks failist, tuhi koht on margistatud _ iga ja eraldatud tuhikuga*)
let read_board filename =
  let board = Array.make_matrix 9 9 0 in
  let ic = open_in filename in
  try
    for i = 0 to 8 do
      let line = input_line ic in
      let cells = String.split_on_char ' ' line in
      for j = 0 to 8 do
        board.(i).(j) <-
          (match List.nth cells j with "_" -> 0 | v -> int_of_string v)
      done
    done;
    close_in ic;
    board
  with e ->
    close_in_noerr ic;
    raise e

(*abifunktsioon laua printimiseks*)
let print_board board =
  print_endline "-------------------------";
  for i = 0 to 8 do
    print_string "| ";
    for j = 0 to 8 do
      print_int board.(i).(j);
      print_string " ";
      if (j + 1) mod 3 = 0 then print_string "| "
    done;
    print_endline "";
    if (i + 1) mod 3 = 0 then print_endline "-------------------------"
  done

(* sudoku lahendaja*)
let solve_sudoku board =
  let ctx = mk_context [ ("model", "true") ] in
  let solver = Solver.mk_simple_solver ctx in
  let cells =
    Array.init 9 (fun i ->
        Array.init 9 (fun j ->
            Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "cell_%d_%d" i j)))
  in
  let cell_constraints =
    List.init 9 (fun i ->
        List.init 9 (fun j ->
            Boolean.mk_and ctx
              [
                Arithmetic.mk_le ctx
                  (Arithmetic.Integer.mk_numeral_i ctx 1)
                  cells.(i).(j);
                Arithmetic.mk_le ctx
                  cells.(i).(j)
                  (Arithmetic.Integer.mk_numeral_i ctx 9);
              ]))
  in
  (* rea piirangud*)
  let row_constraints =
    List.init 9 (fun i -> Boolean.mk_distinct ctx (Array.to_list cells.(i)))
  in
  (* veeru piirangud*)
  let column_constraints =
    List.init 9 (fun j ->
        Boolean.mk_distinct ctx (List.init 9 (fun i -> cells.(i).(j))))
  in
  (* ruudu piirangud *)
  let box_constraints =
    List.init 3 (fun i ->
        List.init 3 (fun j ->
            Boolean.mk_distinct ctx
              (List.init 3 (fun k ->
                   List.init 3 (fun l -> cells.((3 * i) + k).((3 * j) + l)))
              |> List.flatten)))
  in
  (*sisestatud väärtused*)
  let value_constraints =
    List.init 9 (fun i ->
        List.init 9 (fun j ->
            match board.(i).(j) with
            | 0 -> Boolean.mk_true ctx
            | v ->
                Boolean.mk_eq ctx
                  cells.(i).(j)
                  (Arithmetic.Integer.mk_numeral_i ctx v)))
  in
  let status =
    Solver.check solver
      (List.flatten cell_constraints
      @ List.flatten value_constraints
      @ row_constraints @ column_constraints
      @ List.flatten box_constraints)
  in
  match status with
  | Solver.SATISFIABLE ->
      let model = Option.get (Solver.get_model solver) in
      let board =
        Array.map
          (fun row ->
            Array.map
              (fun cell ->
                int_of_expr (Option.get (Model.get_const_interp_e model cell)))
              row)
          cells
      in
      print_board board
  | Solver.UNSATISFIABLE -> print_endline "unsat"
  | Solver.UNKNOWN -> print_endline "unknown"

(*main, kusib failinime*)
let () =
  print_endline "Sudoku solver";
  print_endline "Enter filename:";
  let filename = read_line () in
  let board = read_board filename in
  solve_sudoku board
