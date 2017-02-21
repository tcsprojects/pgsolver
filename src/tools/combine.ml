open Arg ;;
open Tcsargs;;


module CommandLine =
struct
  let input_files = ref []

  let speclist =  []

  let header = Info.get_title "Combine Tool"
end ;;


open CommandLine ;;

let _ =
  SimpleArgs.parsedef speclist (fun f -> input_files := f::!input_files) (header ^ "Usage: combine [infile] [infile] ... [infile]\n" ^
                                              "Combines all specified parity games into one game.");


  let in_channels = List.map open_in !input_files in

  let games = List.map Parsers.parse_parity_game in_channels in

  let game = Transformations.combine_games (List.rev games) in

  Paritygame.print_game game
