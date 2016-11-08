(* This file implements the network service for remote calls to prob *)

open Core.Std
open Async.Std
open Cohttp_async
open Printf

let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/home" -> Server.respond_with_string "This is a home, get out.\n"
  | "/query" ->    Uri.get_query_param uri "ship"
                |> Option.map ~f:(sprintf "So you wanna know about ship %s, eh?\n")
                |> Option.value ~default:"You need to specify a ship, silly.\n"
                |> Server.respond_with_string
  | _       -> Server.respond_with_string "What are you looking for?\n"

let start_server port () =
  printf "Listening for HTTP on port %d\n" port;
  printf "You can try wget or curl with http://localhost:%d/<something>\n%!" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler
  >>= fun _ -> Deferred.never ()


let () = Command.async
  ~summary:"Start the server"
  Command.Spec.(empty +>
    flag "-p" (optional_with_default 8080 int)
      ~doc:"port to listen on"
  ) start_server

  |> Command.run
