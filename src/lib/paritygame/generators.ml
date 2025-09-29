let register_generator = Generatorregistry.register_generator;;

let mem_generator = Generatorregistry.mem_generator;;

let find_generator = Generatorregistry.find_generator;;

let enum_generators = Generatorregistry.enum_generators;;

let fold_generators = Generatorregistry.fold_generators;;

let run_command_line_generator generator =
    let (gen, _) = find_generator generator in
    let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
    let pg = gen args in
    Paritygame.print_game pg;;

let _ =
    Randomgame.register ();
    Laddergame.register ();
    Clusteredrandomgame.register ();
    Cliquegame.register ();
    Modelcheckerladder.register ();
    Recursiveladder.register ();
    Steadygame.register ();
    Jurdzinskigame.register ();
    Elevators.register ();
    Roadworks.register ();
    Towersofhanoi.register ();
    Langincl.register ();
    Recursivedullgame.register ();;
