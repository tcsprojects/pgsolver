open Basics ;;
open Tcsargs;;
open Arg ;;
open Paritygame ;;
open Generators ;;

module CommandLine =
struct
  type generator = NoGenerator
                 | YesGenerator of string * (string array -> paritygame)
  let generator = ref NoGenerator

  let set_verbosity i = if i >=0 && i <= 3 then verbosity := i
  let be_quiet _ = set_verbosity 0
  let be_verbose _ = set_verbosity 2
  let be_very_verbose _ = set_verbosity 3

  let genlist = fold_generators (fun _ ident _ t -> t ^ " " ^ ident) ""
  let genargs = ref []
  let printsolverinfo = ref false

  let speclist =  [ (["-v"], Int(set_verbosity),
                      "<level>\n     sets the verbosity level, valid arguments are 0 (quiet), 1 (default), 2 (verbose) and 3 (very verbose)");
                    (["--generator"; "-gen"], 
                      String (fun s -> let (gen, ident) = find_generator s in generator := YesGenerator (ident, gen)),
                     "<generator> use generator, valid ones are" ^ genlist) ]

  let header = Info.get_title "Parity Game Generator"
end ;;

open CommandLine ;;

let _ =
  Random.self_init ();
  SimpleArgs.parsedef speclist (fun a -> genargs := a :: !genargs) (header ^ "Usage:
    generator --generator <generator family> <args>\n" ^
                                              "Generates parity games to STDOUT.\n");
  let genargs = List.rev !genargs in

  let game =
  match !generator with
  | NoGenerator ->
      failwith "Error: You must specify --generator <generator> <args>."
  | YesGenerator (_, gen) ->
      gen (Array.of_list (genargs))
  in

  flush stdout;
	Paritygame.print_game game;
