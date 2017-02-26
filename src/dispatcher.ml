module type Dispatcher = sig
  val config: Rules_config.t
  val dispatch: string -> Connection.connection option Lwt.t
end

module Make_dispatcher (C:sig val config: Rules_config.t end) : Dispatcher = struct
  let config = C.config

  let dispatch data =
    let _startswith s1 s2 =
      String.sub s1 0 (String.length s2)
      |> String.compare s2 |> (=) 0
    in
    let comparer = _startswith data in
    (* Sexplib.Sexp.to_string (Sexplib.Conv.sexp_of_string data) *)
    (* |> print_endline; *)
    let open Rules_config in
    let rec _loop = function
      | [] -> Lwt.return None
      | {data; address} :: tl when comparer data ->
        let host, port = address in
        Lwt.try_bind
          (fun () -> Connection.connect host port)
          (fun conn -> Lwt.return conn)
          (fun exn ->
             Common.print_exc
               (Printf.sprintf "Could not dispatch to backend %s:%d"
                  host port)
               exn;
             Lwt.return None)
      | _ :: tl -> _loop tl
    in
    _loop config

end

