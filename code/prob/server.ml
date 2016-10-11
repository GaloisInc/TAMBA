(* This file implements the network service for remote calls to prob *)

open Core.Std
open Core.Std.Unix
open Async.Std
open Async_extra
open Cohttp_async
open Json

(* The Async.Pipes that are used as follows:
 *
 *  Writer: The web-request handlers write (string, ivar) pairs to the pipe
 *      They wait for the ivar to be filled and then respond to the request
 *      with it
 *
 *  Reader: The dispatcher thread reads a pair off of the Pipe and sends the
 *      String to the prob process (the dispatcher thread has a separate
 *      _unix_ pipe that it uses to communicate with the prob process).
 *
 *      When the prob process responds the dispatcher thread fills the ivar
 *      the response value
 *)
let (q_reader, q_writer) = Pipe.create ()

(* Logging infrastructure *)
(* why is the current dir in latte_tmp?! that took me a while to figure out
 * for now we'll just hack it.
 * TODO: make less hacky *)
let logger = let output_file = Log.Output.file `Text ("../../qif-server.log") in
             Log.create `Info [output_file]

(* Maintain a list of models in use *)
let models = ref []
 
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
  | Some str -> let exists = List.mem !models str in
                if exists = false then
                  models := str::(!models);
 
                if exists then
                  Server.respond_with_string "model reinitialized\n"
                else 
                  Server.respond_with_string "model initialized\n"

let delete_model uri =
  let m_opt = Uri.get_query_param uri "model" in
  match m_opt with
  | None     -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string "Error: Missing 'model' parameter" in
                Server.respond ~body:body code
  | Some str -> let exists = List.mem !models str in
                if exists then
                  models := List.filter !models (fun x -> x <> str) ;

                if exists then
                  Server.respond_with_string "success\n"
                else 
                  let code = Cohttp.Code.status_of_code 400 in
                  let body = Body.of_string "Error: Invalid 'model' parameter" in
                  Server.respond ~body:body code
 

let list_models ()  =
  let str = "[" ^ (String.concat ~sep:", " !models) ^ "]\n" in
  Server.respond_with_string str

let process_query query_name params uri =
  printf "Prossesing Query\n";
  let (abs, prs) = process_params params uri in
  printf "Length of abs: %d\nlength of prs: %d\n%!" (List.length abs) (List.length prs);
  match (abs, prs) with
  | ([], []) -> raise (Failure "Something when wrong in process_params")
  | ([], xs) -> let serialised = query_to_string query_name prs in
                printf "serialised: %s\n%!" serialised;
                let response_ivar = Async.Std.Ivar.create () in
                Async.Std.Pipe.write q_writer (serialised, response_ivar)
                >>= fun _ -> Async.Std.Ivar.read response_ivar
                >>= fun rsp -> Server.respond_with_string rsp
  | (ys, _)  -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string ("Error: Missing " ^ String.concat ~sep:" " ys ^ " parameters\n") in
                Server.respond ~body:body code

let handler ~body:_ _sock req =
  printf "\nRequest sexp:\n\t%s\n%!" (Core_kernel.Core_sexp.to_string (Cohttp_async.Request.sexp_of_t req));
  let uri = Cohttp.Request.uri req in
  let header = Cohttp.Request.headers req in
  Log.string logger (Uri.to_string uri);
  printf "\nheader: %s\n%!" (Cohttp.Header.to_string header);
  match Uri.path uri with
  | "/home" -> Server.respond_with_string "This is a home, get out.\n"
  | "/query" ->    Uri.get_query_param uri "ship"
                |> Option.map ~f:(sprintf "So you wanna know about ship %s, eh?\n")
                |> Option.value ~default:"You need to specify a ship, silly.\n"
                |> Server.respond_with_string
  | "/InitModel" -> init_model uri
  | "/Distance"  -> process_query "close_enough" distance_params uri
  | "/Resource"  -> process_query "enough_berths" resource_params uri
  | "/Combined"  -> process_query "combined" combined_params uri
  | "/DeleteModel" -> delete_model uri
  | "/ListModels" -> list_models ()
  | _       -> Server.respond_with_string "What are you looking for?\n"

let start_server_prime pid port () =
  (* Preamble for logging *)
  let pid_int = Core.Std.Pid.to_int pid in
  printf "In the parent. Waiting on child with pid %d\n%!" pid_int;
  (* let (_pid, sigl) = Core.Std.Unix.wait (`Pid pid) in
  let sig_str = Core.Std.Unix.Exit_or_signal.to_string_hum sigl in *)
  printf "Listening for HTTP on port %d\n" port;
  printf "You can try wget or curl with http://localhost:%d/<something>\n%!" port;

  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler
  >>= fun _ -> Deferred.never ()

let dispatcher_loop (prob_r, prob_w) =
  (* Create the Async_unix Writer *)
  let fd_w = Async_unix.Fd.create Async_unix.Fd.Kind.Fifo prob_w (Info.of_string "server's write pipe") in
  let prob_w = Async_unix.Writer.create fd_w in

  (* Create the Async_unix Reader *)
  let fd_r = Async_unix.Fd.create Async_unix.Fd.Kind.Fifo prob_r (Info.of_string "server's read pipe") in
  let prob_r = Async_unix.Reader.create fd_r in

  printf "Before dispatcher loop\n%!";
  let rec go () = 
          Pipe.read q_reader >>=
          function
          | `Eof            -> printf "Reached Eof of q_reader\n%!"; return ()
          | `Ok (msg, ivar) -> Async_unix.Writer.write_line prob_w msg;
                               printf "Successfully written to prob_w\n%!";
                               (printf "about to read from prob_r\n%!";
                                Async_unix.Reader.read_line prob_r >>=
                                function
                                | `Eof    -> printf "Reached Eof of prob_r\n%!"; return ()
                                | `Ok rsp -> printf "rsp: %s\n%!" rsp; return (Async.Std.Ivar.fill ivar rsp));
                               go ()
  in go ()

let start_server pid port (r_pipe, w_pipe) () =
  dispatcher_loop (r_pipe, w_pipe);
  start_server_prime pid port ();
  never_returns (Scheduler.go ());
