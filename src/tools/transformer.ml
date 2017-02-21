open Arg ;;
open Tcsargs;;
open Paritygame;;

module CommandLine =
struct
  let todo = ref []
  let input_file = ref ""

  let speclist =  [(["--total"; "-to"], Unit(fun _ -> todo := (fun g -> Transformations.total_transformation_inplace !g)::!todo),
                      "\n     total transformation") ;
                   (["--alternating"; "-al"], Unit(fun _ -> todo := (fun g -> g := Transformations.alternating_transformation !g false)::!todo),
                      "\n     alternating transformation") ;
                   (["--normalform"; "-nf"], Unit(fun _ -> todo := (fun g -> g := Transformations.normal_form_translation !g)::!todo),
                      "\n     transforms game into an equivalent version with out-degree <= 2") ;
                   (["--singlescc"; "-ss"], Unit(fun _ -> todo := (fun g -> g := Transformations.single_scc_transformation !g)::!todo),
                      "\n     single scc transformation") ;
                   (["--prioalignment"; "-pa"], Unit(fun _ -> todo := (fun g -> g := Transformations.prio_alignment_transformation !g)::!todo),
                      "\n     priority alignment transformation") ;
                   (["--dummynodes"; "-dn"], Unit(fun _ -> todo := (fun g -> g := Transformations.dummy_transformation !g)::!todo),
                      "\n     dummy node transformation") ;
                   (["--uniquizeprios"; "-up"], Unit(fun _ -> todo := (fun g -> Transformations.uniquize_prios_inplace !g)::!todo),
                      "\n     uniquize priorities") ;
                   (["--cheapescapecycles"; "-ce"], Unit(fun _ -> todo := (fun g -> g := Transformations.cheap_escape_cycles_transformation !g false)::!todo),
                      "\n     cheap escale cycles transformation");
                   (["--bouncingnode"; "-bn"], Unit(fun _ -> todo := (fun g -> g := Transformations.bouncing_node_transformation !g)::!todo),
                      "\n     bouncing node transformation");
                   (["--increasepriorityoccurrence"; "-ip"], Unit(fun _ -> todo := (fun g -> g := Transformations.increase_priority_occurrence !g)::!todo),
                      "\n     increases priority occurrence by inserting dummy nodes");
                   (["--antiprioritycompactation"; "-ap"], Unit(fun _ -> todo := (fun g -> g := Transformations.anti_priority_compactation_transformation !g)::!todo),
                      "\n     anti priority compactation transformation");
                   (["--minmaxswap"; "-mm"], Unit(fun _ -> todo := (fun g -> g := Transformations.min_max_swap_transformation !g)::!todo),
                      "\n     transforms a min-parity game into a max-parity one and vice versa")]

  let header = Info.get_title "Transformer Tool"
end ;;

open CommandLine ;;


let _ =
  SimpleArgs.parsedef speclist (fun f -> input_file := f) (header ^ "Usage: transformer [options] [infile]\n" ^
                                              "Transforms the parity game given in <infile>. If this argument is omitted it reads a game from STDIN.\n\nOptions are");

  let in_channel = if !input_file = "" then stdin else open_in !input_file in

  let game = ref (Parsers.parse_parity_game in_channel) in

  List.iter (fun f ->
  	f game
  ) (List.rev !todo);

  Paritygame.print_game !game
