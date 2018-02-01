open Tcsset ;; 
(* open Paritygame ;; *)
open Mucalculus ;;
    
let length = ref 0

		
module RoadWorksProblem = 
  struct
    type trafficlight = Red | Green
    let switch = function Red -> Green
			| Green -> Red

    type turn = TrafficLight | Environment
    type roadsegment = CarLeft | CarRight | Empty
				    
    type state = { leftlight: trafficlight;
                   road: roadsegment array;
                   rightlight: trafficlight;
		   turn: turn }
			   
    type proposition = LeftGreen | RightGreen | LeftWait | RightWait | Crash
    let show_proposition = function LeftGreen  -> "lg"
				  | RightGreen -> "rg"
				  | LeftWait   -> "lw"
				  | RightWait  -> "rw"
				  | Crash      -> "*"
								     
    let labels st = function LeftGreen  -> st.leftlight = Green 
			   | RightGreen -> st.rightlight = Green
			   | LeftWait   -> st.road.(0) = CarRight
			   | RightWait  -> st.road.(!length-1) = CarLeft
 			   | Crash -> begin
				        let crash = ref false in
					for i=1 to !length-3 do
					  if st.road.(i) = CarRight && st.road.(i+1) = CarLeft then
					    crash := true
					done;
					!crash
				      end
						      
    type action = Step
    let actions _ = [ Step ]
    let show_action = function Step    -> "."
					 
    let initstate _ = {leftlight = Red; road = Array.make !length Empty; rightlight = Red; turn = Environment }

    let successors st _ =
      if st.turn = TrafficLight then 
        (* traffic lights can change colour or do nothing *)
	[ { st with leftlight = switch st.leftlight;
		    turn = Environment } ;
  	  { st with rightlight = switch st.rightlight;
		    turn = Environment } ;
	  { st with turn = Environment } ]
      else
	begin
	  let succs = ref [ { st with turn = TrafficLight } ] in  

          (* let new cars appear at the edges if there are none *)
	  if st.road.(0) = Empty then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(0) <- CarRight;
				       r);
			       turn = TrafficLight } :: !succs;
	  if st.road.(!length-1) = Empty then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(!length-1) <- CarLeft;
				       r);
			       turn = TrafficLight } :: !succs;
	  
	  (**** one car can move at a time ****)
	  (* the leftmost can only go if the left traffic light is green *)
	  if st.leftlight= Green && st.road.(0) = CarRight && st.road.(1) = Empty then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(0) <- Empty;
				       r.(1) <- CarRight;
				       r);
			       turn = TrafficLight } :: !succs;
	  
	  (* all other cars but the last one trying to go right *)
	  for i=1 to !length-3 do
	    if st.road.(i) = CarRight && st.road.(i+1) = Empty then
	      succs := { st with road = (let r = Array.copy st.road in
					 r.(i) <- Empty;
					 r.(i+1) <- CarRight;
					 r);
				 turn = TrafficLight } :: !succs
	  done;
	  
	  (* the last one going right does not need an empty space to the right *)
	  if st.road.(!length-2) = CarRight then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(!length-2) <- Empty;
				       r);
			       turn = TrafficLight } :: !succs;
	  
	  (* the rightmost can only go if the right traffic light is green *)
	  if st.rightlight= Green && st.road.(!length-1) = CarLeft && st.road.(!length-2) = Empty then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(!length-1) <- Empty;
				       r.(!length-2) <- CarLeft;
				       r);
			       turn = TrafficLight } :: !succs;
	  
	  (* all other cars trying to go left *)
	  for i=2 to !length-2 do
	    if st.road.(i) = CarLeft && st.road.(i-1) = Empty then
	      succs := { st with road = (let r = Array.copy st.road in
					 r.(i) <- Empty;
					 r.(i-1) <- CarLeft;
					 r);
				 turn = TrafficLight } :: !succs
	  done;
	  
	  (* the first one going left does not need an empty space to the left *)
	  if st.road.(1) = CarLeft then
	    succs := { st with road = (let r = Array.copy st.road in
				       r.(1) <- Empty;
				       r);
			       turn = TrafficLight } :: !succs;
	  
	  !succs
	end
	  
    let show_state st =
      let show_car = function CarLeft  -> "<"
			    | CarRight -> ">"
			    | Empty    -> "_"
      in
      let show_light = function Red -> "R"
			      | Green -> "G"
      in
      show_car st.road.(0) ^
	show_light st.leftlight ^
	  String.concat "" (List.map show_car (Array.to_list (Array.sub st.road 1 (!length-2)))) ^
	    show_light st.rightlight ^
	      show_car st.road.(!length-1)


    (* 
     CTL* formula:    ???
     in mu-calculus:  ???  
     *)

    let property _ = Nu("X2", Mu("X1", Conj[ Neg(Prop(Crash)) ;
					     Disj[ Diam(None, Box(None, Var("X1")));
						   Conj[ Disj[ Prop(LeftGreen); Neg(Prop(LeftWait)) ];
						         Disj[ Prop(RightGreen); Neg(Prop(RightWait)) ];
						         Diam(None, Box(None, Var("X2")))
						       ]
					         ]
					   ]
				)
		       )

		     
  end;;

module RWVGame = Make(RoadWorksProblem);;
		   

let roadworks_verification_func arguments = 

  let show_help _ =
    print_string (Info.get_title "Road Works Verification Game Generator");
    print_string ("Usage: roadworksverificationgame n \n\n" ^
		  "       where n = length of shared piece of road in segments (>= 1)\n\n")
  in
  if (Array.length arguments > 1 || Array.length arguments < 1) then (show_help (); exit 1);

  (try
    length := int_of_string arguments.(0)
  with _ -> (show_help (); exit 1));

  RWVGame.build ()


let roadworks_verification_func _ = failwith "Bitte nicht!" 

let register _ = Generatorregistry.register_generator roadworks_verification_func "roadworksvergm" "Road Works Verification Game";;

