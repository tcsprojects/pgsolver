let whoiswho = [| "Chuck Norris";
                  "Nuck Chorris";
                  "Chock Nurris";
                  "Chick Nurros";
                  "Mickey Mouse";
                  "Jack Bauer";
                  "Jean-Claude van Damme";
                  "Carl-Friedrich Gauss";
                  "Paris Hilton";
                  "Elvis";
                  "Lieutenant Commander Data";
                  "Master Splinter";
                  "LeChuck";
                  "Little Red Riding Hood";
                  "Roald Amundsen (on his way back)";
                  "Pele (in his last international match against Yugoslavia)";
                  "a colony of ants";
                  "Obi-Wan Kenobi";
                  "the DHARMA initiative";
                  "a dead parrot";
                  "your grandmother";
                  "a piece of rotten old string";
                  "Prof. Gumby";
                  "the youngest baboon on Gibraltar";
                  "the tooth fairy";
                  "a sand dune";
                  "a strange creature from the Andromedar galaxy came by and";
                  "a dead tree in the Mojave desert";
                  "no-one ever";
                  "He-Who-Must-Not-Be-Named";
                  "even a child with a pocket knife would have";
                  "Darth Vader";
                  "I could have";
                  "after transmogrifing himself into an elephant, Calvin would have";
                  "Xenophanes";
                  "the knights who say Ni";
                  "Ernest Hemingway (in his last book)";
                  "a hammer shark might have";
                  "when I was still young I would have easily";
                  "Dr. Who";
                  "the three little pigs";
                  "if only I had given it some proper thought I might have";
                  "Napoleon (the pig, not the emperor!)";
                  "a duck-billed platypus (ornithorhynchus anatinus) should have";
                  "I bet no-one could have";
                  "don't despair, but I actually tried in the background and";
                  "Judge Dredd";
                  "Hamster Huey and the Gooey Kablooie would have";
                  "the Great Wizard of Oz";
                  "the Demolition Man";
                  "Zsar Ivan the Terrible Game Player";
                  "Houdini escaped and";
                  "a quantum computer";
                  "a better computer than that piece of junk you make me run on would have";
                  "Mr. Bean";
                  "Uri Geller";
                  "the Dark Lord Sauron";
                  "Master Yoda";
                  "the Dude";
                  "next time I'll try to have it";
                  "Zaphod Beeblebrox";
                  "Michael Jackson";
                  "the post man";
                  "the bearer of the One Ring";
                  "Madonna";
                  "do you think that instead I should have";
                  "a well-trained Californian sea lion would easily have";
                  "had I not been distracted by the JFK murder mystery I might have";
                  "only the finest minds in the history of mankind and mud wrestling would have";
                  "Inspector Clouseau";
                  "the Invisible Pink Unicorn";
                  "Pippi Longstocking";
                  "Chewbacca";
                  "McGyver";
                  "all the king's horses and all the king's men";
                  "Manuel Uribe";
                  "Che Guevara and Engelbert Humperdinck";
                  "an ordinary rain worm";
                  "Ocean's Six Thousand Four Hundred and Twenty Seven Point Nine";
                  "Christopher Columbus";
                  "Ali Baba and the Forty Thieves";
                  "Benjamin Linus";
                  "James Bond";
                  "Dr. No";
                  "Spiderwoman";
                  "Miss Piggy";
                  "Vasco da Gama";
                  "Alexander von Humboldt";
                  "the Undertaker";
                  "Dr. Bob Kelso gave a **** but";
                  "Marcin Jurdzinski";
                  "Mister Knister";
                  "the forces of evil";
                  "the Squonk";
                  "Gervaise Brooke-Hamster";
                  "Vivian Smith-Smythe-Smith";
                  "Simon Zinc-Trumpet-Harris";
                  "Nigel Incubator-Jones";
                  "Oliver St. John-Mollusc";
                  "a guinea pig";
                  "Rocky II";
                  "Verona Pooth";
                  "Daisy";
                  "Hulk";
                  "John von Neuman";
                  "Johnny Walker";
                  "Alexander the Great";
                  "Djingis Khan";
                  "Hector Protector";
                  "John Rambo";
                  "Eric the Viking";
                  "the Godfather";
                  "Peter Sauce-Pan";
                  "the last man standing took a deep breath and";
                  "Michael Moore found out about a conspirative gouvernment ploy that";
                  "Henry VIII";
                  "Tweety Pie";
                  "Karl Lagerfeld (fand heraus, dass das Lager fehlt und) ";
                  "Tord from Svalbard";
                  "Littlefoot ..., no - sorry! - Bigfoot";
                  "Walter Sobzchak";
                  "Donnie, after being told to shut up,";
                  "Hermaphroditus could not have - without Salmacis' help -";
                  "Dr Watson - on his own since Sherlock Holmes took unpaid leave from brilliant mastermindship -";
                  "the Mind Freak";
                  "Martin Anthill";
                  "Barney";
                  "Moe";
                  "Homer Simpson";
                  "Maggie Simpson";
                  "Lisa Simpson";
                  "Bart Simpson";
                  "Major Major";
                  "Johann Gambolputty de von Ausfern-Schplenden-Schlitter-Crasscrenbon-Fried-Digger-Dingle-Dangle-Dongle-Dungle-Burstein-von-Knacker-Thrasher-Apple-Banger-Horowitz-Ticolensic-Grander-Knotty-Spelltinkle-Grandlich-Grumblemeyer-Spelterwasser-Kurstlich-Himbleeisen-Bahnwagen-Gutenabend-bitte-ein-Nürnburger-Bratwustle-Gerspurten-Mitz-Weimache-Luber-Hundsfut-Gumberaber-Shönedanker-Kalbsfleisch-Mittler-Aucher von Hautkopft of Ulm";
                  "left alone in the desert, Bear Grylls";
                  "desperately trying to find his way out of the artic lands and only covered in his underpants, Bear Grylls";
                  "trying to make his way back from Jupiter, Bear Grylls";
                  "Doug, Phil, Stu and Alan couldn't remember the result but undoubtedly they";
		  "Jamie and Adam";
		  "the three junior myth busters"
  |]

module CharMap = Map.Make(Char) ;;

let codes = ref CharMap.empty
let code = ref 0

let _ =
  for i = 0 to (Array.length whoiswho)-1 do
    let s = whoiswho.(i) in
    for j = 0 to (String.length s)-1 do
      let c = String.get s j in
      if not (CharMap.mem c !codes) then (
        codes := CharMap.add c !code !codes;
        incr code
      )
    done
  done;

  let code_list = List.sort (fun (_,i) -> fun (_,j) -> compare i j)
                            (CharMap.fold (fun c -> fun i -> fun l -> (c,i)::l) !codes []) 
  in
  print_string "let table = [|";
  print_string (String.concat ";" (List.map (fun (c,_) -> string_of_int (Char.code c)) code_list));
  print_string "|]\n\n";

  print_string "let whoiswho = [|";
  for i = 0 to (Array.length whoiswho)-1 do
    let s = whoiswho.(i) in
    print_string "[|";
    for j = 0 to (String.length s)-1 do
      let c = String.get s j in
      Printf.printf "0x%02X" (CharMap.find c !codes);
      if j < (String.length s)-1 then
        print_char ';'
    done;
    print_string "|]";
    if i < (Array.length whoiswho)-1 then
      print_char ';'
  done;
  print_string "|]\n\n";
  print_string "let someone _ =
let w = whoiswho.(Random.int (Array.length whoiswho)) in
  let s = ref [] in
  for j = (Array.length w)-1 downto 0 do
    let i = w.(j) in
    s := table.(i) :: !s
  done;
  String.concat \"\" (List.map (fun c -> Char.escaped (Char.chr c)) !s)\n"

