(* This file implements the network service for remote calls to prob *)

open Core.Std
open Core.Std.Unix
open Async.Std
open Cohttp_async
open Printf

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
  | Some str -> Server.respond_with_string (Core.Std.Float.to_string (Random.float 1.0))

let distance uri =
  let m_opt = Uri.get_query_param uri "model" in
  match m_opt with
  | None     -> let code = Cohttp.Code.status_of_code 400 in
                let body = Body.of_string "Error: Missing 'model' parameter" in
                Server.respond ~body:body code
  | Some str -> Server.respond_with_string (Core.Std.Float.to_string (Random.float 1.0))

let handler writer ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/home" -> Async_unix.Writer.write writer "We got something!\n";
               Async_unix.Writer.write writer "We got something else!\n";
               Server.respond_with_string "This is a home, get out.\n"
  | "/query" ->    Uri.get_query_param uri "ship"
                |> Option.map ~f:(sprintf "So you wanna know about ship %s, eh?\n")
                |> Option.value ~default:"You need to specify a ship, silly.\n"
                |> Server.respond_with_string
  | "/InitModel" -> init_model uri
  | "/Distance"  -> distance uri
  (*| "/Resource"  -> resource uri
  | "/Combined"  -> combined uri *)
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
