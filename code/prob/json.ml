open Yojson
open Globals
open Cohttp_async
open Printf

(* Taking a query's name and the parameters for that query
 * create a JSON value and serialize to a string
 *
 * Note: the validation of the query needs to happen before
 *       this function is called
 *)
let query_to_string query_name params =
  let query_json = ("query", `String query_name) in

  (* if the query is 'Resource' or 'Combined' we need to encode the
   * resource type as an int. This is done with the following map
   *)
  let resource_map r = match r with
                       | "berths" -> 0
                       | "water"  -> 1
                       | "food"   -> 2
                       | "kits"   -> 3 in

  let param_to_json (name, value) =
    match name with
    | "resource" -> (name, `Int (resource_map value))
    | v          -> (name, `Int (int_of_string value)) in
  Yojson.to_string (`Assoc (query_json :: List.map param_to_json params))

(* Take the string-serialized version of the JSON representing a query
 * and give back a pair with the query name and the in-list
 *)
let parse_query_json str =
  let json_of_query = Yojson.Basic.from_string str in
  let query_name = json_of_query |> Basic.Util.member "query" |> Basic.Util.to_string in
  
  (* A function that gets a single parameter from the json and
   * makes it an Int
   *)
  let get_param str = ifdebug (printf "get_param %s\n%!" str);
                      (str, json_of_query |> Basic.Util.member str |> Basic.Util.to_int) in

  (* get the list of parameters (i.e. everything except "query" *)
  let param_list = List.filter (fun x -> x <> "query" && x <> "resource")
                               (Basic.Util.keys json_of_query) in
  ifdebug (printf "param_list:";
           let _ = List.map (fun x -> printf " %s," x) param_list in
           printf "\n%!");
  let inlist = List.map get_param param_list in
  let mkvid (str, v) = (("", str), v) in
  ifdebug (printf "here\n%!");
  (query_name, List.map mkvid inlist)
