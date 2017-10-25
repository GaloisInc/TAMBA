open Yojson
open Globals
open Cohttp_async
open Printf
open Util

let int_of_bool_string str =
  match str with
  | "true"  -> 1
  | "false" -> 0
  | "True"  -> 1
  | "False" -> 0
  | s       -> try int_of_string s
               with e -> raise (Failure "Could not parse result argument")

(* I (JMCT) am so angry that I have to write the following function.
 * This shouldn't even be a thing.
 * Real World OCaml lied about how Yojson handles string value!
 *
 * This function removes the opening a closing quotes from json string
 *)
let rem_quotes json =
  let str     = Basic.to_string json in
  let len     = String.length str in
  let no_open = String.sub str 1 (len - 1) in
  let no_quot = String.sub no_open 0 (len - 2) in
  no_quot

(* Taking a query's name and the parameters for that query
 * create a JSON value and serialize to a string
 *
 * Note: the validation of the query needs to happen before
 *       this function is called
 *)
let query_to_string query_name params : string option =
  ifdebug (printf "query_name: %s%!\n" query_name);
  let query_json = ("query", `String query_name) in

  (* if the query is 'Resource' or 'Combined' we need to encode the
   * resource type as an int. This is done with the following map *)
  let resource_map r = match r with
                       | "berths" -> 0
                       | "water"  -> 1
                       | "food"   -> 2
                       | "kits"   -> 3 in

  let param_to_json (name, value) =
    ifdebug (printf "param: %s\tvalue: %s\n%!" name value);
    match name with
    | "resource" -> (name, `Int (resource_map value))
    | "result"   -> (name, `Int (int_of_string value))
    | "static"   -> (name, `Int (int_of_bool_string value))
    | "ids"      -> (name, Basic.from_string value)
    | "ship_lat" -> (name, `Int (int_of_float (float_of_string value *. 1000.0)))
    | "ship_long" -> (name, `Int (int_of_float (float_of_string value *. 1000.0)))
    | "port_lat" -> (name, `Int (int_of_float (float_of_string value *. 1000.0)))
    | "port_long" -> (name, `Int (int_of_float (float_of_string value *. 1000.0)))
    | "tolerance" -> (name, `Int (int_of_string value))
    | v          -> (name, `Int (int_of_string value)) in
  try Some (Yojson.Basic.to_string (`Assoc (query_json :: List.map param_to_json params)))
  with e ->
    printf "%s\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    None

(* Take the string-serialized version of the JSON representing a query
 * and give back a 4-tuple with the following:
 *
 *  1. The name of the query to be run
 *  2. The list of intput values for that query
 *  3. The model under test
 *  4. An option with the provided resulting value (if the query requires it)
 *)
let parse_query_json str =
  let json_of_query = Yojson.Basic.from_string str in
  ifdebug (printf "Now using YoJSON structure\n%!");
  let query_name = json_of_query |> Basic.Util.member "query" |> Basic.Util.to_string in

  ifdebug (printf "We now have query_name\n%!");
  (* A function that gets a single parameter from the json and
   * makes it an Int *)
  let get_param str = ifdebug (printf "get_param %s\n%!" str);
                      (str, json_of_query |> Basic.Util.member str |> Basic.Util.to_int) in

  let model = json_of_query |> Basic.Util.member "model" |> Basic.Util.to_int in
  ifdebug (printf "We now the model number\n%!");
  (* check if this query is meant to be static *)
  let static = match Basic.Util.member "static" json_of_query with
              | `Null  -> false
              | `Int 0 -> false
              | `Int 1 -> true in
  let tolerance = match Basic.Util.member "tolerance" json_of_query with
              | `Null  -> 1
              | `Int r -> r in
  let res   = match Basic.Util.member "result" json_of_query with
              | `Null  -> if static then (ifdebug (printf "Static!\n%!"); Static tolerance) else RunConc
              | `Int r -> (ifdebug (printf "Dynamic!%!"); Dynamic r) in
  (* get the list of parameters (i.e. everything except
   * "query", "resource", "result", or "model" *)
  let param_list = List.filter (fun x -> x <> "query" &&
                                         x <> "resource" &&
                                         x <> "tolerance" &&
                                         x <> "model" &&
                                         x <> "static" &&
                                         x <> "ids" &&
                                         x <> "result")
                               (Basic.Util.keys json_of_query) in
  ifdebug (printf "param_list:";
           let _ = List.map (fun x -> printf " %s," x) param_list in
           printf "\n%!");
  let inlist = List.map get_param param_list in
  let ids = match Basic.Util.member "ids" json_of_query with
            | `Null    -> []
            | `List xs -> List.map rem_quotes xs in
  ifdebug (printf "ids: %s\n%!" (String.concat ", " ids));
  let mkvid (str, v) = (("", str), v) in
  ifdebug (printf "qn: %s\n%!" query_name);
  (query_name, List.map mkvid inlist, model, ids, tolerance, res)
