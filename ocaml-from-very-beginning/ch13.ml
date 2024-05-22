let channel_statistics : in_channel -> unit =
 fun chan ->
  let lines = ref 0 in
  let sentences = ref 0 in
  let words = ref 0 in
  let characters = ref 0 in
  try
    while true do
      let w = input_line chan in
      lines := !lines + 1;
      String.iter
        (function
          | '.' | '!' | '?' ->
              sentences := !sentences + 1;
              words := !words + 1
          | ' ' -> words := !words + 1
          | _ -> characters := !characters + 1)
        w
    done
  with End_of_file ->
    print_endline "Text statistics:";
    Printf.printf "\tLines:           %d\n" !lines;
    Printf.printf "\tSentences:       %d\n" !sentences;
    Printf.printf "\tWords:           %d\n" !words;
    Printf.printf "\tCharacters:      %d\n" !characters;
    if !words > 0 then
      let cpw : float = float_of_int !characters /. float_of_int !words in
      Printf.printf "\tAvg word length: %.2f characters\n" cpw
    else ();
    print_newline ()

let file_statistics : string -> unit =
 fun name ->
  let chan = open_in name in
  try
    channel_statistics chan;
    close_in chan
  with _ -> close_in chan
