
let parse_args () =
  let port = ref 9000 in
  let config = ref "" in
  Arg.parse [
      ("", Arg.Set_string config, "config file");
      ("-port", Arg.Set_int port, "port number, default = 9000");
    ] (fun conf_ -> config := conf_) "";
  if "" = !config then
    raise (Failure "Invalid config file")
  else
    !port, !config


let () =
  let port, config = parse_args () in
  let config = Rules_config.load config in
  let module Dispatcher = Dispatcher.Make_dispatcher(struct let config=config end) in
  let module Proxy = Proxy.Make_proxy(Dispatcher) in
  Proxy.start port
