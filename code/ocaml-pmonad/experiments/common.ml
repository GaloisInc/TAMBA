let arg =
  [("--debug",
    Arg.Set Util.debug, "debug mode")];;

let parse opts ep = 
  Arg.parse (arg @ opts)
    (fun s -> ()) "";
;;
