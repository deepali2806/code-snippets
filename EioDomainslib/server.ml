open Eio.Std

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 45

module T = Domainslib.Task

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b


(* let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081) *)
let addr = `Unix "/tmp/eio-test.sock"

let read_all flow =
  let b = Buffer.create 100 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
  Buffer.contents b

exception Graceful_shutdown

let run_client ~sw ~net ~addr =
  traceln "Connecting to server...";
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string "Hello from client" flow;
  Eio.Flow.shutdown flow `Send;
  let msg = read_all flow in
  traceln "Client received: %S" msg

let run_server ~sw socket ~domain_mgr =
  while true do
    Eio.Net.accept_sub socket ~sw (fun ~sw flow _addr ->
      traceln "Server accepted connection from client";
      let _ = Switch.check sw in ();
      Fun.protect (fun () ->
        let msg = read_all flow in
        traceln "Server received: %S" msg;
      let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
    let n = Eio.Domain_manager.run domain_mgr (fun () -> (fib_par pool n)) in

        (* let n = fib_par pool 45 in  *)
        traceln "Fibonacci value %d" n
      ) ~finally:(fun () -> Eio.Flow.copy_string "Bye" flow)
    )
    ~on_error:(function
      | Graceful_shutdown -> ()
      | ex -> traceln "Error handling connection: %s" (Printexc.to_string ex)
    );
  done

let test_address addr ~net ~domain_mgr sw=
  let server = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in

  Fibre.both
    (fun () -> run_server ~sw server ~domain_mgr)
    (fun () ->
        let t1 = Unix.gettimeofday () in
      run_client ~sw ~net ~addr;
      traceln "Client finished - cancelling server";
      let t2 = Unix.gettimeofday () in
  traceln "Total Time Difference for fibonacci %f" (t2-.t1);

      raise Graceful_shutdown
    )
    
(* (fn : net:Eio.Net.t -> Switch.t -> unit) *)
(* let run  = *)
  (* let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in *)
  

let () =
Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  Switch.run (test_address addr ~net ~domain_mgr)