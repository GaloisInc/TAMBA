(* This file implements the network service for remote calls to prob *)

open Core.Std
open Core.Std.Unix
open Async.Std
open Async_extra
open Cohttp_async

(* Logging infrastructure *)
(* why is the current dir in latte_tmp?! that took me a while to figure out
 * for now we'll just hack it.
 * TODO: make less hacky *)
let logger = let output_file = Log.Output.file `Text ("../../qif-server.log") in
             Log.create `Info [output_file]

(* Define the parameter lists for each possible query *)
(* TODO: This should be determined by the query itself, which will require
 * some reorganisation of the code. Two possible strategies:
 *
 *  1) parse the query file before forking and pass the 'policy system' to the server
 *  2) use another pipe to communicate the possible queries and params
 *)
let distance_params = ["model"; "ship"; "eta"; "result"]
let resource_params = ["model"; "ship"; "resouce"; "amount"; "result"]
let combined_params = ["model"; "ship"; "resouce"; "amount"; "eta"; "result"]

(* A specialized 'either' type for looking up parameters in a URI
 *
 * We retain the name of what was looked up instead of useing an option
 * type that would hide that from us
 *)
type param = Absent of string
           | Present of string * string

let process_params xs uri =
  (* lookup up the parameter in the URI
   * if it's there, then mark as present and keep the value
   * if it's not, mark as absent *)
  let proc_param str =
    match Uri.get_query_param uri str with
    | None   -> Absent str
    | Some v -> Present (str, v) in

  let rec split (a, p) zs =
    match zs with
    | []      -> (a, p)
    | (y::ys) -> (match y with
                  | Absent s      -> split (s::a, p       ) ys
                  | Present (s,v) -> split (a   , (s,v)::p) ys) in
  split ([], []) (List.map xs proc_param)

let init_model uri =
  let m_opt = Uri.get_query_param uri "model" in
  match m_opt with
  | None     -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string "Error: Missing 'model' parameter" in
                Server.respond ~body:body code
  | Some str -> Server.respond_with_string "success\n"

let delete_model uri =
  let m_opt = Uri.get_query_param uri "model" in
  match m_opt with
  | None     -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string "Error: Missing 'model' parameter" in
                Server.respond ~body:body code
  | Some str -> Server.respond_with_string "success\n"

let process_query params uri =
  printf "Prossesing Query\n";
  let (abs, prs) = process_params params uri in
  printf "Length of abs: %d\nlength of prs: %d" (List.length abs) (List.length prs);
  match (abs, prs) with
  | ([], []) -> raise (Failure "Something when wrong in process_params")
  | ([], xs) -> Server.respond_with_string (Core.Std.Float.to_string (Random.float 1.0))
  | (ys, _)  -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string ("Error: Missing " ^ String.concat ~sep:" " ys ^ " parameters\n") in
                Server.respond ~body:body code

let handler writer ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  Log.string logger (Uri.to_string uri);
  match Uri.path uri with
  | "/home" -> Async_unix.Writer.write writer "We got something!\n";
               Async_unix.Writer.write writer "We got something else!\n";
               Server.respond_with_string "This is a home, get out.\n"
  | "/query" ->    Uri.get_query_param uri "ship"
                |> Option.map ~f:(sprintf "So you wanna know about ship %s, eh?\n")
                |> Option.value ~default:"You need to specify a ship, silly.\n"
                |> Server.respond_with_string
  | "/InitModel" -> init_model uri
  | "/Distance"  -> process_query distance_params uri
  | "/Resource"  -> process_query resource_params uri
  | "/Combined"  -> process_query combined_params uri
  | "/DeleteModel" -> delete_model uri
  | _       -> Server.respond_with_string "What are you looking for?\n"

let start_server_prime pid port w_pipe () =
  (* Preamble for logging *)
  let pid_int = Core.Std.Pid.to_int pid in
  printf "In the parent. Waiting on child with pid %d\n%!" pid_int;
  (* let (_pid, sigl) = Core.Std.Unix.wait (`Pid pid) in
  let sig_str = Core.Std.Unix.Exit_or_signal.to_string_hum sigl in *)
  printf "Listening for HTTP on port %d\n" port;
  printf "You can try wget or curl with http://localhost:%d/<something>\n%!" port;

  (* Create the Asyn_unix Writer *)
  let fd = Async_unix.Fd.create Async_unix.Fd.Kind.Fifo w_pipe (Info.of_string "server's write pipe") in
  let writer = Async_unix.Writer.create fd in
  
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) (handler writer)
  >>= fun _ -> Deferred.never ()

let start_server pid port w_pipe () =
  start_server_prime pid port w_pipe ();
  never_returns (Scheduler.go ());
