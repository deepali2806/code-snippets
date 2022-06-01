open Eio.Std
(* open Eio_linux *)
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

let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
  
  Eio_main.run @@ fun _env ->
  let promise, resolver = Promise.create () in
  let clock = Eio.Stdenv.clock _env in
  let t1 = Eio.Time.now clock in

  Switch.run (fun sw ->

  Fibre.fork ~sw 
  (fun () -> let t1 = Eio.Time.now clock in
    traceln "Waiting for parallel answer = %d" x;   
    let x = Promise.await promise in   
    let t2 = Eio.Time.now clock in
    traceln "Waiting Time for Parallel fibonacci %f" (t2-.t1)
  );


  Fibre.fork ~sw
    (fun () -> let t1 = Eio.Time.now clock in
          traceln "Calling parallel fibonacci";
    let _ = Promise. fulfill resolver (fib_par pool n) in
          traceln "Parallel Fibonacci is done";
          let t2 = Eio.Time.now clock in
     traceln "Time Difference for Parallel fibonacci %f" (t2-.t1)
    );

  
  Fibre.fork ~sw 
  (fun () -> 
  traceln "Calling Sequential fibonacci";
  let t1 = Eio.Time.now clock in traceln "Sequential Fibonacci = %d" (fib 45);   
     let t2 = Eio.Time.now clock in
     traceln "Time Difference for sequential fibonacci %f" (t2-.t1)
  )

  );
  (* let res = T.run pool (fun _ -> fib_par pool n) in *)
  let t2 = Eio.Time.now clock in
  traceln "Total Time Difference for fibonacci %f" (t2-.t1);
  T.teardown_pool pool

(* 
let square n = n * n

let x = 5
let y = 10

let _ =
  let d = Domain.spawn (fun _ -> square x) in
  let sy = square y in
  Eio_main.run @@ fun _env ->
  Fibre.both
    (fun () ->  let sx = try Domain.join d with _ -> 12 in traceln "x^2 = %d" sx )
    (fun () -> traceln "y = %d" sy) *)