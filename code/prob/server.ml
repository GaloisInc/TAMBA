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
let (log_in, logger) =
  let mk_log x = let output_file = Log.Output.rotating_file `Text x (Log.Rotation.default ()) in
                 Log.create `Info [output_file] in
  (mk_log "../../logs/qif-requests", mk_log "../../logs/qif-responses")

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
let resource_params = ["model"; "ship"; "resource"; "amount"; "result"]
let combined_params = ["model"; "ship"; "resource"; "amount"; "eta"; "result"]

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

let handle_impossible uri host =
  let code = Cohttp.Code.status_of_code 500 in
  let rsp = "Error: The query resulted in an impossible state.\n" in
  let body = Body.of_string rsp in
  Log.string logger (host ^ " " ^ Uri.to_string uri ^ " " ^ "500" ^ " " ^ rsp);
  Server.respond ~body:body code

let pass_to_prob json uri host =
    ifdebug (printf "serialised: %s\n%!" json);
    let response_ivar = Async.Std.Ivar.create () in
    Async.Std.Pipe.write q_writer (json, response_ivar)
    >>= fun _ -> Async.Std.Ivar.read response_ivar
    >>= fun rsp -> if rsp = "-inf"
                   then handle_impossible uri host
                   else (Log.string logger (host ^ " " ^ Uri.to_string uri ^ " " ^ "200" ^ " " ^ rsp);
                         Server.respond_with_string (rsp ^ "\n"))

let handle_local cmd str =
  let exists = List.mem !models str in
    match cmd with
    | "init_model" ->
        if exists = false then
          models := str::(!models)
    | "delete_model" ->
        if exists then
          models := List.filter !models (fun x -> x <> str)

let unsafe_get_param uri param =
  match Uri.get_query_param uri param with
  | None     -> raise (Failure ("unsafe_get_param was used on param " ^ param))
  | Some str -> str

let ensure_parse uri host cmd =
  match cmd with
  | None -> let code = Cohttp.Code.status_of_code 400 in
            let rsp = "Error: Could not parse query" in
            let body = Body.of_string (rsp ^ "\n") in
            Log.string logger (host ^ " " ^ Uri.to_string uri ^ " " ^ "400" ^ " " ^ rsp);
            Server.respond ~body:body code
  | Some str -> pass_to_prob str uri host


let handle_models cmd uri host =
  let m_opt = Uri.get_query_param uri "model" in
  match m_opt with
  | None     -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string "Error: Missing 'model' parameter\n" in
                Server.respond ~body:body code
  | Some str -> handle_local cmd str;
                let serialised = query_to_string cmd [("model", str)] in
                ensure_parse uri host serialised

let list_models ()  =
  let str = "[" ^ (String.concat ~sep:", " !models) ^ "]\n" in
  Server.respond_with_string str

let process_query query_name params uri host =
  ifdebug (printf "Prossesing Query\n");
  let (abs, prs) = process_params params uri in
  ifdebug (printf "Length of abs: %d\nlength of prs: %d\n%!" (List.length abs) (List.length prs));
  match (abs, prs) with
  | ([], []) -> raise (Failure "Something when wrong in process_params")
  | ([], xs) -> let serialised = query_to_string query_name prs in
                (* use of unsafe_get_param should be okay because we have no
                 * absent parameters (matching on (abs, prs)) *)
                let exists = List.mem !models (unsafe_get_param uri "model") in
                if exists
                then ensure_parse uri host serialised
                else let code = Cohttp.Code.status_of_code 400 in
                     let rsp = "Error: Trying to work on non-existent model" in
                     let body = Body.of_string (rsp ^ "\n") in
                     Log.string logger (host ^ " " ^ Uri.to_string uri ^ " " ^ "400" ^ " " ^ rsp);
                     Server.respond ~body:body code
  | (ys, _)  -> let code = Cohttp.Code.status_of_code 400 in
                let rsp = "Error: Missing " ^ String.concat ~sep:" " ys ^ " parameters" in
                let body = Body.of_string (rsp ^ "\n") in
                Log.string logger (host ^ " " ^ Uri.to_string uri ^ " " ^ "400" ^ " " ^ rsp);
                Server.respond ~body:body code

let handler ~body:_ _sock req =
  ifdebug (printf "\nRequest sexp:\n\t%s\n%!"
                  (Core_kernel.Core_sexp.to_string (Cohttp_async.Request.sexp_of_t req)));
  let uri = Cohttp.Request.uri req in
  let host = match Uri.host uri with
             | None   -> "0.0.0.0"
             | Some v -> v in
  Log.string log_in (host ^ " " ^ Uri.to_string uri);
  let header = Cohttp.Request.headers req in
  ifdebug (printf "\nheader: %s\n%!" (Cohttp.Header.to_string header));
  match Uri.path uri with
  | "/home" -> Server.respond_with_string "This is a home, get out.\n"
  | "/query" ->    Uri.get_query_param uri "ship"
                |> Option.map ~f:(sprintf "So you wanna know about ship %s, eh?\n")
                |> Option.value ~default:"You need to specify a ship, silly.\n"
                |> Server.respond_with_string
  | "/InitModel"   -> handle_models "init_model" uri host
  | "/Distance"    -> process_query "close_enough" distance_params uri host
  | "/Resource"    -> process_query "enough_berths" resource_params uri host
  | "/Combined"    -> process_query "combined" combined_params uri host
  | "/DeleteModel" -> handle_models "delete_model" uri host
  | "/ListModels"  -> list_models ()
  | _       -> Server.respond_with_string "Error: Unsupported API Call.\n"

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

  ifdebug (printf "Before dispatcher loop\n%!");
  let rec go () =
          Pipe.read q_reader >>=
          function
          | `Eof            -> ifdebug (printf "Reached Eof of q_reader\n%!"); return ()
          | `Ok (msg, ivar) -> Async_unix.Writer.write_line prob_w msg;
                               ifdebug (printf "Successfully written to prob_w\n%!");
                                Async_unix.Reader.read_line prob_r >>=
                                function
                                | `Eof    -> ifdebug (printf "Reached Eof of prob_r\n%!");
                                             return ()
                                | `Ok rsp -> ifdebug (printf "rsp: %s\n%!" rsp);
                                             return (Async.Std.Ivar.fill ivar rsp);
                               go ()
  in go ()

let start_server pid port (r_pipe, w_pipe) () =
  dispatcher_loop (r_pipe, w_pipe);
  start_server_prime pid port ();
  never_returns (Scheduler.go ());
