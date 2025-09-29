(* open Paritygame;; *)
open Tcsset;;

let generatormap = ref TreeMap.empty_def;;

let register_generator generator_func identifier description =
	if TreeMap.mem identifier !generatormap
	then failwith ("generator `" ^ identifier ^ "' already registered!\n")
	else generatormap := TreeMap.add identifier (generator_func, description) !generatormap;;
	
let mem_generator identifier = TreeMap.mem identifier !generatormap;;

let find_generator identifier = TreeMap.find identifier !generatormap;;

let enum_generators it = TreeMap.iter (fun i (f, d) -> it f i d) !generatormap;;

let fold_generators fo b = TreeMap.fold (fun i (f, d) x -> fo f i d x) !generatormap b;;
