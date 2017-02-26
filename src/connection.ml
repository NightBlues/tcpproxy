let (>>=) = Lwt.(>>=)

			  
type connection = {
    buf: Bytes.t;
    buf_size: int;
    sock: Lwt_unix.file_descr;
    addr: Lwt_unix.sockaddr
  }

type proxy_data = {
    client: connection;
    server: connection
}


let _socket () =
  Unix.socket Unix.PF_INET Unix.SOCK_STREAM
    (Unix.getprotobyname "tcp").Unix.p_proto
  |> Lwt_unix.of_unix_file_descr

let _idclient addr =
  match addr with
  | Lwt_unix.ADDR_INET (a, port) ->
     Printf.sprintf
       "%s:%d\n"
       (Unix.string_of_inet_addr a) port
  | _ -> Printf.sprintf "Not tcp..."


let create_connection sock addr =
  let buf_size = 256 in
  let buf = Bytes.create buf_size in
  let conn = {sock; addr; buf; buf_size} in
  conn

let close_connection conn =
  Printf.printf "Closing connection with %s" (_idclient conn.addr);
  flush stdout;
  Lwt_unix.close conn.sock


let connect host port =
  (* todo: error monad *)
  let clsock = _socket () in
  Lwt_unix.getaddrinfo
    host (string_of_int port)
    [Lwt_unix.AI_FAMILY Lwt_unix.PF_INET;
     Lwt_unix.AI_SOCKTYPE Lwt_unix.SOCK_STREAM]
  >>=
    (function
     | addr_inet :: _ ->
        Lwt_unix.connect clsock addr_inet.Lwt_unix.ai_addr
        >>=
          (fun () ->
            Some (create_connection clsock addr_inet.Lwt_unix.ai_addr) |>
              Lwt.return)
     | _ -> Lwt.return None)


let send conn data =
  (* todo: error monad *)
  match conn with
  | None -> Lwt.return None
  | Some conn ->
    Lwt.try_bind
      (fun () -> Lwt_unix.send conn.sock data 0 (Bytes.length data) [])
      (function
        | 0 -> Lwt.return None
        | _ -> Lwt.return (Some conn))
      (fun exn ->
         Common.print_exc
           (Printf.sprintf "Could not proxy data to %s"
              (_idclient conn.addr))
           exn;
         Lwt.return None)
      
