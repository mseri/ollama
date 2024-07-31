open Lwt
open Cohttp
open Cohttp_lwt_unix

(** Type alias for the model name *)
type model = string

(** Type alias for the prompt text *)
type prompt = string

(** Record type representing the response from the Ollama API *)
type response = {
  model: string;
  created_at: string;
  response: string;
  done_: bool;
  context: int list;
  total_duration: int;
  load_duration: int;
  prompt_eval_duration: int;
  eval_duration: int;
  eval_count: int;
}

(** The base URL for the Ollama API *)
let base_url = "http://localhost:11434/api"

(** 
    Generate a stream of responses from the Ollama API
    @param model The name of the model to use
    @param prompt The input prompt for generation
    @return A Lwt stream of response chunks
  *)
let generate_stream ~model ~prompt =
  let uri = Uri.of_string (base_url ^ "/generate") in
  let body = 
    `Assoc [
      ("model", `String model);
      ("prompt", `String prompt);
      ("stream", `Bool true)
    ]
    |> Yojson.Safe.to_string
    |> Cohttp_lwt.Body.of_string
  in
  let headers = Header.init_with "Content-Type" "application/json" in

  Client.post ~headers ~body uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if code <> 200 then
    Lwt.fail_with ("HTTP error: " ^ string_of_int code)
  else
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    Lwt.return (String.split_on_char '\n' body_str |> List.filter (fun s -> s <> ""))

(**
    Parse a JSON response into a response record
    @param json The JSON response from the API
    @return A response record
  *)
let parse_response json =
  let open Yojson.Safe.Util in
  {
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    created_at = json |> member "created_at" |> to_string_option |> Option.value ~default:"";
    response = json |> member "response" |> to_string_option |> Option.value ~default:"";
    done_ = json |> member "done" |> to_bool_option |> Option.value ~default:false;
    context = (try json |> member "context" |> to_list |> List.map to_int with _ -> []);
    total_duration = json |> member "total_duration" |> to_int_option |> Option.value ~default:0;
    load_duration = json |> member "load_duration" |> to_int_option |> Option.value ~default:0;
    prompt_eval_duration = json |> member "prompt_eval_duration" |> to_int_option |> Option.value ~default:0;
    eval_duration = json |> member "eval_duration" |> to_int_option |> Option.value ~default:0;
    eval_count = json |> member "eval_count" |> to_int_option |> Option.value ~default:0;
  }

(**
    Generate responses from the Ollama API
    @param model The name of the model to use
    @param prompt The input prompt for generation
    @return A Lwt stream of parsed response records
  *)
let generate ~model ~prompt =
  generate_stream ~model ~prompt >>= fun chunks ->
  Lwt.return (List.filter_map (fun chunk ->
    try
      Some (Yojson.Safe.from_string chunk |> parse_response)
    with _ ->
      Printf.eprintf "Failed to parse JSON: %s\n" chunk;
      None
  ) chunks)
