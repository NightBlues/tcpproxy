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


