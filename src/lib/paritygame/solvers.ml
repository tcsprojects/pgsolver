let register_solver_factory = Solverregistry.register_solver_factory

let register_solver = Solverregistry.register_solver

let mem_solver = Solverregistry.mem_solver

let find_solver = Solverregistry.find_solver

let enum_solvers = Solverregistry.enum_solvers

let fold_solvers = Solverregistry.fold_solvers

let register_partial_solver_factory = Solverregistry.register_partial_solver_factory

let register_partial_solver = Solverregistry.register_partial_solver

let mem_partial_solver = Solverregistry.mem_partial_solver

let find_partial_solver = Solverregistry.find_partial_solver

let enum_partial_solvers = Solverregistry.enum_partial_solvers

let fold_partial_solvers = Solverregistry.fold_partial_solvers

let _ =
    Bigstep.register ();
    Dominiondecomp.register ();
    Externalsolver.register ();
    Fpiter.register ();
    Genetic.register ();
    Guessstrategy.register ();
    Localmodelchecker.register ();
    Optstratimprov.register ();
    Prioprom.register ();
    Priopromplus.register ();
    Priopromdelay.register ();
    Priopromrecovery.register ();
    Recursive.register ();
    Satsolve.register ();
    Smallprogress.register ();
    (*Smtsolve.register ();*)
    Stratimprdisc.register ();
    Stratimprlocal.register ();
    Stratimprlocal2.register ();
    Stratimprovement.register ();
    Stratimprsat.register ();
    Viasat.register ();
    Stratimpralgs.register ();
    Switch_cunningham.register ();
    Switch_globally_best.register ();
    Switch_history.register ();
    Switch_internal.register ();
    Switch_locally_best.register ();
    Switch_random.register ();
    Switch_snare.register ();
    Switch_zadeh.register ();
    Succinctsmallprogress.register ();;
