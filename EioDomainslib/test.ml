(* open Eio_linux
open Eio.Std *)
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 43
module T = Domainslib.Task

(* 
let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b *)

let createMake () = Atomic.make [];;
  let a = createMake ();;

  let x1 = (Atomic.get a) in
  let _ = 
    (match x1 with
   | [] -> Atomic.set a (1::x1); List.iter (Printf.printf "%d ") (Atomic.get a) 
   | _::_ -> ()
   ) in 
  let oldcontensts = Atomic.get a in
   let ret = (Atomic.compare_and_set a oldcontensts ([1;2]) ) in

    if ret then Printf.printf "Checked"
    else Printf.printf "Not same"




(* let m = Eio_domainslib_interface.MVar.create_empty ();; *)

(* let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in

    run @@ fun _env ->
    T.run pool (fun () -> 
    Switch.run @@ fun sw ->
    (       

            let pr = T.async pool (fun () -> 
                Printf.printf "\nInside Domainslib";
                let p = Eio_domainslib_interface.MVar.take m in 
                Printf.printf "\nDomainslib ended %d" p
            ) in ();
            (* Unix.sleep(2); *)
            Fiber.fork ~sw
            (fun () -> 
            let v = 42 in
            traceln "\nInside Second fibre ";
            let _ = Eio_domainslib_interface.MVar.put v m in 
                        T.await pool pr;

            (* traceln "\n Second Fibre Ends %d " p;  *)
            traceln "\n Second Fibre Ends "; 
            );          
            
      );
    );
    T.teardown_pool pool *)


(* let _ = Eio_domainslib_interface.MVar.check ()  *)
(* 
module S = Eio.Stream

exception Cancel

let add t v =
  traceln "Adding %d to stream" v;
  S.add t v;
  traceln "Added %d to stream" v

let take t =
  traceln "Reading from stream";
  traceln "Got %d from stream" (S.take t)


let main = Eio_linux.run @@ fun _env ->
  Switch.run (fun sw ->
    let t = S.create 1 in
    Fiber.fork ~sw
      (fun () -> take t);
    traceln "First thread forked";
    Fiber.fork ~sw
      (fun () -> raise Cancel);
    traceln "Second thread forked; top-level code is finished"
  );
  traceln "Switch is finished";; *)
