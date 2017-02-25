let (>>=) = Lwt.(>>=)

module Make_proxy (
    D: sig
      val dispatch: string -> Connection.connection option Lwt.t
    end) = struct


let dispatch = D.dispatch

let rec on_chunk autosrv conn connsrv len =
  let data = Bytes.sub conn.Connection.buf 0 len in
  (* Printf.printf "Read %d bytes\n" len; *)
  match len with
  | 0 -> Connection.close_connection conn
  | _ ->
     (match autosrv, connsrv with
       | true, None ->
         (* todo: start reverse receive *)
         let data = Bytes.copy data
                    |> Bytes.escaped
                    |> Bytes.to_string in
         let newconn = dispatch data in
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


let start port =
  let sock = Connection._socket () in
  Lwt_main.run (
      Lwt.try_bind
        (* (fun () -> (Lwt_unix.Versioned.bind_2 sock (Unix.ADDR_INET (Unix.inet_addr_any, port)))) *)
        (fun () -> (Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port))) |> Lwt.return)
        (fun () ->
          Lwt_unix.listen sock 10; acceptor sock ())
        (fun exn -> print_endline "Could not bind to port"; Lwt.return ()))
  
end
