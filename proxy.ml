let (>>=) = Lwt.(>>=)


let dispatch data =
  let _startswith s1 s2 =
    String.sub s1 0 (String.length s2)
    |> String.equal s2
  in
  if _startswith data "GET" then (
    print_endline "http";
    Connection.connect "127.0.0.1" 10000
  ) else
    Lwt.return None


let rec on_chunk autosrv conn connsrv len =
  let data = Bytes.sub_string conn.Connection.buf 0 len in
  (* |> Bytes.escaped *)
  (* |> Bytes.to_string in *)
  (* Printf.printf "Read %d bytes\n" len; *)
  match len with
  | 0 -> Connection.close_connection conn
  | _ ->
     (match autosrv, connsrv with
      | true, None ->
         (* todo: start reverse receive *)
         let newconn = dispatch (Bytes.copy data) in
         let _start_recevier () =
           (newconn >>=
              (function
               | None -> Lwt.return ()
               | Some connsrv ->
                  receive false connsrv (Some conn)))
         in
         Lwt.async _start_recevier;
         newconn
      | false, None -> Lwt.return None
      | _, Some s -> Lwt.return (Some s))
     >>=
       (fun connsrv -> Connection.send connsrv data)
     >>=
       (fun connsrv ->
         match connsrv with
         | None -> Connection.close_connection conn
         | Some connsrv -> receive false conn (Some connsrv))
and receive autosrv conn connsrv =
  let open Connection in
  Lwt_unix.recv conn.sock conn.buf 0 conn.buf_size []
  >>=
    on_chunk true conn connsrv


let reactor (sock, addr) () =
  Printf.printf "accepted connection from %s\n" (Connection._idclient addr);
  let conn = Connection.create_connection sock addr in
  receive true conn None


let rec acceptor sock () =
  Lwt_unix.accept sock
  >>=
    (fun conn ->
      Lwt.async (reactor conn);
      acceptor sock ())


let () =
  let port = 9000 in
  let sock = Connection._socket () in
  Lwt_main.run (
      Lwt.try_bind
        (fun () -> (Lwt_unix.Versioned.bind_2 sock (Unix.ADDR_INET (Unix.inet_addr_any, port))))
        (fun () ->
          Lwt_unix.listen sock 10; acceptor sock ())
        (fun exn -> print_endline "Could not bind to port"; Lwt.return ()))
  
