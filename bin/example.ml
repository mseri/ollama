open Lwt.Syntax

let example_usage () =
  let model = "phi3" in
  let prompt = "Explain the theory of relativity in simple terms." in

  Printf.printf "Generating response for prompt: %s\n" prompt;
  Printf.printf "Using model: %s\n\n" model;

  let* responses = Ollama.generate ~model ~prompt in
  if List.length responses = 0 then
    Lwt_io.printf "No valid responses received.\n"
  else
    let accumulated_response = ref "" in
    let chunk_count = ref 0 in
    let* () =
    Lwt_list.iter_s (fun response ->
      accumulated_response := !accumulated_response ^ response.Ollama.response;
      incr chunk_count;
      if !chunk_count mod 10 = 0 then  (* Print a dot every 10 chunks *)
        let* () = Lwt_io.printf "." in
        Lwt_io.flush Lwt_io.stdout
      else
        Lwt.return_unit
    ) responses in
    Lwt_io.printf "\n\nFull response:\n%s\n\nGeneration complete.\n" !accumulated_response

let () = Lwt_main.run (example_usage ())

