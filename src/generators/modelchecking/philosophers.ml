open Tcsset ;;
  
let n = ref 5

module Philosophers =
  struct
    type proposition = IsHungry of int
		     | IsEating of int

    type action = PickupRight of int
		| PickupLeft of int
		| PutdownRight of int
		| PutdownLeft of int

    type philosopher = Eating | Thinking | HasLeft | HasRight
    type state = philosopher  array

    let initstate = Array.init !n (fun i -> Thinking)

    let labeling s =
      let t = Treeset.empty_def ref in
      Array.iteri (fun i p -> match p with
				 Hungry -> t := Treeset.add t (isHungry i)
			       | Eating -> t := Treeset.add t (isEating i)
			       | _      -> ())
		  s;
      !t

    let successors s =
      let left i = (i-1) mod !n in
      let right i = (i+1) mod !n in
      let update t j p = (Array.copy t).(j) <- p in
      function PickupLeft i -> (let i' = left i in
				if s.(i') != HasRight && s.(i') != Eating then
				  if s.(i) = HasRight then
				    [ update s i Eating ] 
				  else if s.(i) = Thinking then
				    [ update s i HasLeft ]
				  else
				    []
				else
				  [])
	     | PickupRight i -> (let i' = right i in
				if s.(i') != HasLeft && s.(i') != Eating then
				  if s.(i) = HasLeft then
				    [ update s i Eating ] 
				  else if s.(i) = Thinking then
				    [ update s i HasRight ]
				  else
				    []
				else
				  [])
	     | PutdownRight i -> (match s.(i) with
				    HasRight -> [ update s i Thinking ]
				  | Eating -> [ update s i HasLeft ]
				  | _ -> [])
	     | PutdownLeft i -> (match s.(i) with
				    HasLeft -> [ update s i Thinking ]
				  | Eating -> [ update s i HasRight ]
				  | _ -> [])

    let action_list =
      let rec actlst i acc = if i < 0 then acc else actlst (i-1) (PickupLeft i :: PickupRight i :: PutdownLeft i :: PutdownRight i :: acc)
      in
      actlst (!n-1) []
	     
    let show_state s = Array.fold_left (fun t -> fun p -> t ^ (match p with Thinking -> "_"
									  | HasLeft -> "\\"
									  | HasRight -> "/"
									  | Eating -> "*"))
				       "" s
  end;;
