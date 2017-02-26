

let print_exc mymsg e =
  let msg = Printexc.to_string e
  and stack = Printexc.get_backtrace () in
  Printf.eprintf "%s: %s%s\n" mymsg msg stack;
  flush stderr
