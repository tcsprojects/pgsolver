open Paritygame ;;
open Parsers ;;
open Univsolve ;;
open Basics;;

let pipe : in_channel -> out_channel -> unit = fun c_in c_out ->
  let size = 4 * 1024 in
  let buffer = Bytes.create size in
  let eof = ref false in
  while not !eof do
    let len = input c_in buffer 0 size in
    if len > 0
    then output_string c_out (Bytes.sub_string buffer 0 len)
    else eof := true
  done;;

let solve' : Solverregistry.global_solver_factory = fun args game ->
  let cmd = Array.get args 0 in
  let (c_out, c_in, c_err) : in_channel * out_channel * in_channel = Unix.open_process_full cmd [||] in
  let thread =  (Thread.create (fun _ -> pipe c_err stderr) ()) in
  output_game c_in game;
  close_out c_in;
  let sol  = parse_solution c_out in
  close_in c_out;
  Thread.join thread;
  let ret = Unix.close_process_full (c_out, c_in, c_err) in
  ignore ret;
  sol ;;
let solve  : Solverregistry.global_solver_factory = fun args ->
  universal_solve (universal_solve_init_options_verbose !universal_solve_global_options) (solve' args);;
let register _ =
  Solverregistry.register_solver_factory solve "external_solver_univ" "esuniv" "directly solve by calling an executable";
  Solverregistry.register_solver_factory solve' "external_solver" "es" "directly solve by calling an executable";;
